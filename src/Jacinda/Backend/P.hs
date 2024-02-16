{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Backend.P ( EvalErr (..), runJac, eB ) where

import           A
import           A.I
import           Control.Exception          (Exception, throw)
import           Control.Monad              (foldM, (<=<))
import           Control.Monad.State.Strict (State, evalState, get, modify, runState)
import           Data.Bifunctor             (bimap)
import qualified Data.ByteString            as BS
import           Data.Containers.ListUtils  (nubOrdOn)
import           Data.Foldable              (traverse_)
import qualified Data.IntMap                as IM
import           Data.List                  (scanl', transpose, uncons, unzip4)
import           Data.Maybe                 (catMaybes, mapMaybe)
import qualified Data.Vector                as V
import           Data.Word                  (Word8)
import           Foreign.C.String           (CString)
import           Jacinda.Backend.Const
import           Jacinda.Backend.Parse
import           Jacinda.Backend.Printf
import           Jacinda.Fuse
import           Jacinda.Regex
import           Nm
import           Prettyprinter              (hardline, pretty)
import           Prettyprinter.Render.Text  (putDoc)
import           Regex.Rure                 (RureMatch (RureMatch), RurePtr)
import           System.IO                  (hFlush, stdout)
import           System.IO.Unsafe           (unsafeDupablePerformIO)
import           Ty.Const
import           U

φ1 :: E T -> Int
φ1 (BB (TyArr _ (TyArr (TyApp (TyB TyStream) _) _)) Fold1) = 1
φ1 (EApp _ e0 e1)                                          = φ1 e0+φ1 e1
φ1 (Tup _ es)                                              = sum (φ1<$>es)
φ1 (OptionVal _ (Just e))                                  = φ1 e
φ1 (Cond _ p e0 e1)                                        = φ1 p+φ1 e0+φ1 e1
φ1 (Lam _ _ e)                                             = φ1 e
φ1 _                                                       = 0


φ :: E T -> Int
φ (TB (TyArr _ (TyArr _ (TyArr (TyApp (TyB TyStream) _) _))) Fold) = 1
φ (EApp _ e0 e1)                                                   = φ e0+φ e1
φ (Tup _ es)                                                       = sum (φ<$>es)
φ (OptionVal _ (Just e))                                           = φ e
φ (Cond _ p e0 e1)                                                 = φ p+φ e0+φ e1
φ (Lam _ _ e)                                                      = φ e
φ _                                                                = 0

noleak :: E T -> Bool
noleak e = φ e > 1 && φ1 e < 1

runJac :: RurePtr -- ^ Record separator
       -> Bool -- ^ Flush output?
       -> Int
       -> E T
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac re f i e = ϝ (bsProcess re f) (if noleak e then fuse i e else (e, i)) where ϝ = uncurry.flip

data StreamError = NakedField deriving (Show)

instance Exception StreamError where

data EvalErr = EmptyFold
             | IndexOutOfBounds Int
             | InternalCoercionError (E T) TB
             | ExpectedTup (E T)
             | BadHole (Nm T)
             deriving (Show)

instance Exception EvalErr where

(!) :: V.Vector a -> Int -> a
v ! ix = case v V.!? ix of {Just x  -> x; Nothing -> throw $ IndexOutOfBounds ix}

parseAsEInt :: BS.ByteString -> E T
parseAsEInt = mkI.readDigits

parseAsF :: BS.ByteString -> E T
parseAsF = mkF.readFloat

readFloat :: BS.ByteString -> Double
readFloat = unsafeDupablePerformIO . (`BS.useAsCString` atof)

foreign import ccall unsafe atof :: CString -> IO Double

the :: BS.ByteString -> Word8
the bs = case BS.uncons bs of
    Nothing                -> error "Empty splitc char!"
    Just (c,b) | BS.null b -> c
    Just _                 -> error "Splitc takes only one char!"

asTup :: Maybe RureMatch -> E T
asTup Nothing                = OptionVal undefined Nothing
asTup (Just (RureMatch s e)) = OptionVal undefined (Just (Tup undefined (mkI . fromIntegral <$> [s, e])))

mkFoldVar :: Int -> b -> E b
mkFoldVar i l = Var l (Nm "fold_placeholder" (U i) l)

takeConcatMap :: (a -> [b]) -> [a] -> [b]
takeConcatMap f = concat . transpose . fmap f

-- this relies on all streams being the same length stream which in turn relies
-- on the fuse step (fold-of-filter->fold)
foldAll :: Int -> RurePtr -> [(Int, E T, E T, E T)] -> [BS.ByteString] -> ([(Int, E T)], Int)
foldAll i r xs bs = runState (foldMultiple seeds streams ctxStream ixStream) i
    where (ns, ops, seeds, es) = unzip4 xs
          mkStream e = eStream i r e bs
          streams = mkStream<$>es
          ctxStream = [(b, splitBy r b) | b <- bs]
          ixStream = [1..]

          foldMultiple seedsϵ esϵ (ctx:ctxes) (ix:ixes) = allHeads esϵ `seq` do {es' <- sequence$zipWith3 (c2Mϵ (pure.eCtx ctx ix)) ops seedsϵ (head<$>esϵ); foldMultiple es' (tail<$>esϵ) ctxes ixes}
          -- TODO: sanity check same length all streams
          foldMultiple seedsϵ _ [] _ = pure$zip ns seedsϵ

          allHeads = foldr seq ()

gf :: E T -> State (Int, [(Int, E T, E T, E T)]) (E T)
gf (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) stream) | t@(TyApp (TyB TyStream) _) <- eLoc stream = do
    (i,_) <- get
    modify (bimap (+1) ((i, op, seed, stream) :))
    pure $ mkFoldVar i t
gf (EApp ty e0 e1) = EApp ty <$> gf e0 <*> gf e1
gf (Tup ty es) = Tup ty <$> traverse gf es
gf (Arr ty es) = Arr ty <$> traverse gf es
gf (OptionVal ty e) = OptionVal ty <$> traverse gf e
gf (Cond ty p e e') = Cond ty <$> gf p <*> gf e <*> gf e'
gf (Lam t n e) = Lam t n <$> gf e
gf e@BB{} = pure e; gf e@TB{} = pure e; gf e@UB{} = pure e; gf e@NB{} = pure e
gf e@Lit{} = pure e
gf e@RC{} = pure e; gf e@Var{} = pure e

ug :: IM.IntMap (E T) -> E T -> E T
ug st (Var _ n@(Nm _ (U i) _)) =
    IM.findWithDefault (throw (BadHole n)) i st
ug _ e = e

bsProcess :: RurePtr
          -> Bool -- ^ Flush output?
          -> Int -- ^ Unique context
          -> E T
          -> Either StreamError ([BS.ByteString] -> IO ())
bsProcess _ _ _ AllField{} = Left NakedField
bsProcess _ _ _ Field{}    = Left NakedField
bsProcess _ _ _ (NB _ Ix)  = Left NakedField
bsProcess r f u e | (TyApp (TyB TyStream) _) <- eLoc e = Right (pS f.eStream u r e)
bsProcess r f u (Anchor _ es) = Right (\bs -> pS f $ takeConcatMap (\e -> eStream u r e bs) es)
bsProcess r _ u e =
    Right $ \bs -> pDocLn (eF u r e bs)

pDocLn = putDoc.(<>hardline).pretty

pS p = traverse_ g where g | p = (*>fflush).pDocLn | otherwise = pDocLn
                         fflush = hFlush stdout

scanM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanM op seed xs = sequence $
    scanl' go (pure seed) xs where go seedϵ x = do {seedϵ' <- seedϵ; op seedϵ' x}

eF :: Int -> RurePtr -> E T -> [BS.ByteString] -> E T
eF u r e | noleak e = \bs ->
    let (eHoley, (_, folds)) = runState (gf e) (0, [])
        (filledHoles, u') = foldAll u r folds bs
        in eB u' (pure.ug (IM.fromList filledHoles)) eHoley
        | otherwise = \bs ->
        eB u (go bs) e
            where go bb (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) = do
                      op' <- eBM pure op
                      seed' <- eBM pure seed
                      let xsϵ=eStream u r xs bb
                      foldM (c2M op') seed' xsϵ
                  go bb (EApp _ (EApp _ (BB _ Fold1) op) xs) = do
                      op' <- eBM pure op
                      let (seed',xsϵ)=case uncons $ eStream u r xs bb of {Just s -> s; Nothing -> throw EmptyFold}
                      foldM (c2M op') seed' xsϵ
                  go _ eϵ = pure eϵ


a1 :: E T -> E T -> UM (E T)
a1 f x | TyArr _ cod <- eLoc f = lβ (EApp cod f x)

a2 :: E T -> E T -> E T -> UM (E T)
a2 op x0 x1 | TyArr _ t@(TyArr _ t') <- eLoc op = lβ (EApp t' (EApp t op x0) x1)

c1 :: Int -> E T -> E T -> E T
c1 i f x = evalState (eBM pure =<< a1 f x) i

c2M op x0 x1 = eBM pure =<< a2 op x0 x1
c2Mϵ f g e e' = eBM f =<< a2 g e e'

c2 :: Int -> E T -> E T -> E T -> E T
c2 i op x0 x1 = evalState (c2M op x0 x1) i

eStream :: Int -> RurePtr -> E T -> [BS.ByteString] -> [E T]
eStream u r (EApp _ (EApp _ (EApp _ (TB _ Scan) op) seed) xs) bs =
    let op'=eB u pure op; seed'=eB u pure seed; xsϵ=eStream u r xs bs
    in evalState (scanM (c2M op') seed' xsϵ) u
eStream i r (EApp _ (UB _ CatMaybes) e) bs = mapMaybe asM$eStream i r e bs
eStream u r (Implicit _ e) bs = zipWith (\fs i -> eB u (pure.eCtx fs i) e) [(b, splitBy r b) | b <- bs] [1..]
eStream _ _ AllColumn{} bs = mkStr<$>bs
eStream _ _ IParseAllCol{} bs = parseAsEInt<$>bs
eStream _ _ FParseAllCol{} bs = parseAsF<$>bs
eStream _ _ (ParseAllCol (TyApp _ (TyB TyInteger))) bs = parseAsEInt<$>bs
eStream _ _ (ParseAllCol (TyApp _ (TyB TyFloat))) bs = parseAsF<$>bs
eStream _ r (Column _ i) bs = mkStr.(! (i-1)).splitBy r<$>bs
eStream _ r (IParseCol _ n) bs = [parseAsEInt (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (ParseCol (TyApp _ (TyB TyInteger)) n) bs = [parseAsEInt (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (FParseCol _ n) bs = [parseAsF (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (ParseCol (TyApp _ (TyB TyFloat)) n) bs = [parseAsF (splitBy r b ! (n-1)) | b <- bs]
eStream i r (EApp _ (EApp _ (BB _ MapMaybe) f) e) bs = let xs = eStream i r e bs in mapMaybe (asM.c1 i f) xs
eStream i r (EApp _ (EApp _ (BB _ Map) f) e) bs = let xs=eStream i r e bs in fmap (c1 i f) xs
eStream i r (EApp _ (EApp _ (BB _ Prior) op) e) bs = let xs=eStream i r e bs in zipWith (c2 i op) (tail xs) xs
eStream i r (EApp _ (EApp _ (BB _ Filter) p) e) bs = let xs=eStream i r e bs; ps=fmap (asB.c1 i p) xs in [x | (pϵ,x) <- zip ps xs, pϵ]
eStream i r (EApp _ (EApp _ (EApp _ (TB _ ZipW) f) e0) e1) bs = let xs0=eStream i r e0 bs; xs1=eStream i r e1 bs in zipWith (c2 i f) xs0 xs1
eStream i r (EApp (TyApp _ (TyB TyStr)) (UB _ Dedup) e) bs = let s = eStream i r e bs in nubOrdOn asS s
eStream i r (EApp (TyApp _ (TyB TyInteger)) (UB _ Dedup) e) bs = let s = eStream i r e bs in nubOrdOn asI s
eStream i r (EApp (TyApp _ (TyB TyFloat)) (UB _ Dedup) e) bs = let s = eStream i r e bs in nubOrdOn asF s
eStream i r (EApp _ (EApp _ (BB _ DedupOn) op) e) bs | TyArr _ (TyB TyStr) <- eLoc op = let xs = eStream i r e bs in nubOrdOn (asS.c1 i op) xs
eStream i r (EApp _ (EApp _ (BB _ DedupOn) op) e) bs | TyArr _ (TyB TyInteger) <- eLoc op = let xs = eStream i r e bs in nubOrdOn (asI.c1 i op) xs
eStream i r (EApp _ (EApp _ (BB _ DedupOn) op) e) bs | TyArr _ (TyB TyFloat) <- eLoc op = let xs = eStream i r e bs in nubOrdOn (asF.c1 i op) xs
eStream u r (Guarded _ p e) bs =
    let bss=(\b -> (b, splitBy r b))<$>bs
    in catMaybes $ zipWith (\fs i -> if asB (eB u (pure.eCtx fs i) p) then Just (eB u (pure.eCtx fs i) e) else Nothing) bss [1..]

asS :: E T -> BS.ByteString
asS (Lit _ (StrLit s)) = s; asS e = throw (InternalCoercionError e TyStr)

asI :: E T -> Integer
asI (Lit _ (ILit i)) = i; asI e = throw (InternalCoercionError e TyInteger)

asF :: E T -> Double
asF (Lit _ (FLit x)) = x; asF e = throw (InternalCoercionError e TyFloat)

asR :: E T -> RurePtr
asR (RC r) = r; asR e = throw (InternalCoercionError e TyR)

asM :: E T -> Maybe (E T)
asM (OptionVal _ e) = e; asM e = throw (InternalCoercionError e TyOption)

asB :: E T -> Bool
asB (Lit _ (BLit b)) = b; asB e = throw (InternalCoercionError e TyBool)

asV :: E T -> V.Vector (E T)
asV (Arr _ v) = v; asV e = throw (InternalCoercionError e TyVec)

asT :: E T -> [E T]
asT (Tup _ es) = es; asT e = throw (ExpectedTup e)

vS :: V.Vector BS.ByteString -> E T
vS = Arr (tyV tyStr).fmap mkStr

eCtx :: (BS.ByteString, V.Vector BS.ByteString) -- ^ Line, split by field separator
     -> Integer -- ^ Line number
     -> E T -> E T
eCtx ~(f, _) _ AllField{}  = mkStr f
eCtx (_, fs) _ (Field _ i) = mkStr (fs ! (i-1))
eCtx (_, fs) _ LastField{} = mkStr (V.last fs)
eCtx (_, fs) _ FieldList{} = vS fs
eCtx _ i (NB _ Ix)         = mkI i
eCtx (_, fs) _ (NB _ Nf)   = mkI (fromIntegral$V.length fs)
eCtx _ _ e                 = e

eB :: Int -> (E T -> UM (E T)) -> E T -> E T
eB i f x = evalState (eBM f x) i

{-# SCC eBM #-}
eBM :: (E T -> UM (E T)) -> E T -> UM (E T)
eBM f (EApp t (EApp _ (EApp _ (TB _ Captures) s) i) r) = do
    s' <- eBM f s; i' <- eBM f i; r' <- eBM f r
    pure $ OptionVal t (mkStr <$> findCapture (asR r') (asS s') (fromIntegral$asI i'))
eBM f (EApp t (EApp _ (EApp _ (TB _ AllCaptures) s) i) r) = do
    s' <- eBM f s; i' <- eBM f i; r' <- eBM f r
    pure $ Arr t (V.fromList (mkStr <$> captures' (asR r') (asS s') (fromIntegral$asI i')))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Max) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (max x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Min) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (min x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Min) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (min x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Max) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (max x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Min) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkStr (min x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Max) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkStr (max x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Plus) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (x0'+x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Minus) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (x0'-x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Times) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (x0'*x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Plus) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'+x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Minus) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'-x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Times) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'*x1'))
eBM f (EApp _ (EApp _ (BB _ Div) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'/x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Eq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Neq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'/=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Gt) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'>x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Lt) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'<x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Leq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'<=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Geq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'>=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Gt) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'>x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Lt) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'<x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Eq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Neq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'/=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Geq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'>=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Leq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'<=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Eq) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Neq) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkB (x0'/=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Gt) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkB (x0'>x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Geq) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkB (x0'>=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Lt) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkB (x0'<x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Leq) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkB (x0'<=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Exp) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (x0'^x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Exp) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'**x1'))
eBM f (EApp _ (EApp _ (BB (TyArr (TyApp (TyB TyVec) t) _) Eq) x0) x1) = do
    x0' <- asV<$>eBM f x0; x1' <- asV<$>eBM f x1
    mkB <$> if V.length x0'==V.length x1'
        then all asB <$> V.zipWithM (c2Mϵ f op) x0' x1'
        else pure False
    where op = BB (TyArr t (TyArr t tyB)) Eq
eBM f (EApp _ (EApp _ (BB (TyArr (TyApp (TyB TyOption) t) _) Eq) x0) x1) = do
    x0' <- asM<$>eBM f x0; x1' <- asM<$>eBM f x1
    case (x0',x1') of
        (Nothing, Nothing) -> pure (mkB True)
        (Nothing, Just{})  -> pure (mkB False)
        (Just{}, Nothing)  -> pure (mkB False)
        (Just e0, Just e1) -> c2Mϵ f op e0 e1
    where op = BB (TyArr t (TyArr t tyB)) Eq
eBM f (EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Plus) x0) x1) = do
    x0' <- asS <$> eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkStr (x0'<>x1'))
eBM f (EApp _ (EApp _ (BB _ And) x0) x1) = do
    x0' <- asB<$>eBM f x0; x1' <- asB<$>eBM f x1
    pure (mkB (x0'&&x1'))
eBM f (EApp _ (EApp _ (BB _ Or) x0) x1) = do
    x0' <- asB<$>eBM f x0; x1' <- asB<$>eBM f x1
    pure (mkB (x0'||x1'))
eBM f (EApp _ (UB _ Not) b) = do {b' <- asB<$>eBM f b; pure $ mkB (not b')}
eBM f (EApp _ (EApp _ (BB _ Matches) s) r) = {-# SCC "eBMMatch" #-} do
    s' <- asS<$>eBM f s; r' <- asR<$>eBM f r
    pure $ mkB (isMatch' r' s')
eBM f (EApp _ (EApp _ (BB _ NotMatches) s) r) = do
    s' <- asS<$>eBM f s; r' <- asR<$>eBM f r
    pure $ mkB (not$isMatch' r' s')
eBM f (EApp _ (EApp _ (BB _ Split) s) r) = do
    s' <- asS<$>eBM f s; r' <- asR<$>eBM f r
    pure (vS (splitBy r' s'))
eBM f (EApp _ (EApp _ (BB _ Splitc) s) c) = do
    s' <- asS<$>eBM f s; c' <- the.asS<$>eBM f c
    pure (vS (V.fromList (BS.split c' s')))
eBM f (EApp _ (UB _ FParse) x) = do {x' <- eBM f x; pure (parseAsF (asS x'))}
eBM f (EApp _ (UB _ IParse) x) = do {x' <- eBM f x; pure (parseAsEInt (asS x'))}
eBM f (EApp (TyB TyInteger) (UB _ Parse) x) = do {x' <- eBM f x; pure (parseAsEInt (asS x'))}
eBM f (EApp (TyB TyFloat) (UB _ Parse) x) = do {x' <- eBM f x; pure (parseAsF (asS x'))}
eBM f (EApp _ (UB _ (At i)) v) = do {v' <- eBM f v; pure (asV v'!(i-1))}
eBM f (EApp _ (UB _ (Select i)) x) = do {x' <- eBM f x; pure (asT x' !! (i-1))}
eBM f (EApp _ (UB _ Floor) x) = do {xr <- asF<$>eBM f x; pure $ mkI (floor xr)}
eBM f (EApp _ (UB _ Ceiling) x) = do {xr <- asF<$>eBM f x; pure $ mkI (ceiling xr)}
eBM f (EApp (TyB TyInteger) (UB _ Negate) i) = do {i' <- eBM f i; pure $ mkI (negate (asI i'))}
eBM f (EApp (TyB TyFloat) (UB _ Negate) x) = do {x' <- eBM f x; pure $ mkF (negate (asF x'))}
eBM f (EApp t (UB _ Some) e) = do {e' <- eBM f e; pure (OptionVal t (Just e'))}
eBM _ (NB t None) = pure (OptionVal t Nothing)
eBM f (EApp _ (UB _ Tally) e) = do
    s' <- eBM f e
    let r =fromIntegral (BS.length$asS s')
    pure (mkI r)
eBM f (EApp _ (UB _ TallyList) e) = do
    e' <- eBM f e
    let r=fromIntegral (V.length$asV e')
    pure (mkI r)
eBM f (EApp _ (EApp _ (UB _ Const) e) _) = eBM f e
eBM f (EApp _ (EApp _ (BB _ Sprintf) fs) s) = do
    fs' <- eBM f fs; s' <- eBM f s
    pure $ mkStr (sprintf (asS fs') s')
eBM f (EApp _ (EApp _ (BB _ Match) s) r) = do
    s' <- eBM f s; r' <- eBM f r
    pure $ asTup (find' (asR r') (asS s'))
eBM f (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) | TyApp (TyB TyVec) _ <- eLoc xs = do
    op' <- eBM f op; seed' <- eBM f seed; xs' <- eBM f xs
    V.foldM (c2Mϵ f op') seed' (asV xs')
eBM f (EApp _ (EApp _ (BB _ Fold1) op) xs) | TyApp (TyB TyVec) _ <- eLoc xs = do
    op' <- eBM f op; xs' <- eBM f xs
    let xsV=asV xs'; Just (seed, xs'') = V.uncons xsV
    V.foldM (c2Mϵ f op') seed xs''
eBM f (EApp yT@(TyApp (TyB TyOption) _) (EApp _ (BB _ Map) g) x) = do
    g' <- eBM f g; x' <- eBM f x
    OptionVal yT <$> traverse (eBM f <=< a1 g') (asM x')
eBM f (EApp yT@(TyApp (TyB TyVec) _) (EApp _ (BB _ Map) g) x) = do
    g' <- eBM f g; x' <- eBM f x
    Arr yT <$> traverse (eBM f <=< a1 g') (asV x')
eBM f (EApp t (EApp _ (EApp _ (TB _ Option) x) g) y) = do
    x' <- eBM f x; g' <- eBM f g; y' <- eBM f y
    case asM y' of
        Nothing -> pure x'
        Just yϵ -> eBM f =<< lβ (EApp t g' yϵ)
eBM f (EApp _ (EApp _ (EApp _ (TB _ Substr) s) i0) i1) = do
    i0' <- eBM f i0; i1' <- eBM f i1; s' <- eBM f s
    pure $ mkStr (substr (asS s') (fromIntegral$asI i0') (fromIntegral$asI i1'))
eBM f (Cond _ p e e') = do {p' <- eBM f p; if asB p' then eBM f e else eBM f e'}
eBM f (Tup t es) = Tup t <$> traverse (eBM f) es
eBM f e = f e
