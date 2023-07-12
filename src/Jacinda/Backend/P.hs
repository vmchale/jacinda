{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Backend.P ( runJac, eB ) where

import           A
import           A.I
import           Control.Exception          (Exception, throw)
import           Control.Monad              (foldM)
import           Control.Monad.State.Strict (State, evalState, get, modify, runState)
import           Data.Bifunctor             (bimap)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as ASCII
import           Data.Containers.ListUtils  (nubOrdOn)
import           Data.Foldable              (traverse_)
import qualified Data.IntMap                as IM
import           Data.List                  (scanl', unzip4)
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Vector                as V
import           Data.Word                  (Word8)
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
import           Ty.Const
import           U

runJac :: RurePtr -- ^ Record separator
       -> Bool -- ^ Flush output?
       -> Int
       -> E (T K)
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac re f i e = ϝ (bsProcess re f) (fuse i e) where ϝ = uncurry.flip

data StreamError = NakedField
                 deriving (Show)

instance Exception StreamError where

data EvalError = EmptyFold
               | IndexOutOfBounds Int
               | InternalCoercionError (E (T K)) TB
               | ExpectedTup (E (T K))
               | BadHole (Nm (T K))
               deriving (Show)

instance Exception EvalError where

(!) :: V.Vector a -> Int -> a
v ! ix = case v V.!? ix of {Just x  -> x; Nothing -> throw $ IndexOutOfBounds ix}

parseAsEInt :: BS.ByteString -> E (T K)
parseAsEInt = mkI . readDigits

parseAsF :: BS.ByteString -> E (T K)
parseAsF = FLit tyF . readFloat

readFloat :: BS.ByteString -> Double
readFloat = read . ASCII.unpack

the :: BS.ByteString -> Word8
the bs = case BS.uncons bs of
    Nothing                -> error "Empty splitc char!"
    Just (c,b) | BS.null b -> c
    Just _                 -> error "Splitc takes only one char!"

asTup :: Maybe RureMatch -> E (T K)
asTup Nothing                = OptionVal undefined Nothing
asTup (Just (RureMatch s e)) = OptionVal undefined (Just (Tup undefined (mkI . fromIntegral <$> [s, e])))

mkFoldVar :: Int -> b -> E b
mkFoldVar i l = Var l (Nm "fold_placeholder" (U i) l)

-- this relies on all streams being the same length stream which in turn relies
-- on the fuse step (fold-of-filter->fold)
foldAll :: Int -> RurePtr -> [(Int, E (T K), E (T K), E (T K))] -> [BS.ByteString] -> ([(Int, E (T K))], Int)
foldAll i r xs bs = runState (foldMultiple seeds (mkStreams es)) i
    where (ns, ops, seeds, es) = unzip4 xs
          mkStreams = fmap (\e -> eStream i r e bs)

          foldMultiple seedsϵ esϵ | not (any null esϵ) = do {es' <- sequence$zipWith3 c2M ops seedsϵ (head<$>esϵ); foldMultiple es' (tail<$>esϵ)}
                                  | otherwise = pure$zip ns seedsϵ

gf :: E (T K) -> State (Int, [(Int, E (T K), E (T K), E (T K))]) (E (T K))
gf (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) stream) | TyApp _ (TyB _ TyStream) _ <- eLoc stream = do
    (i,_) <- get
    modify (bimap (+1) ((i, op, seed, stream) :))
    pure $ mkFoldVar i undefined
gf (EApp ty e0 e1) = EApp ty <$> gf e0 <*> gf e1
gf (Tup ty es) = Tup ty <$> traverse gf es
gf (Arr ty es) = Arr ty <$> traverse gf es
gf (OptionVal ty e) = OptionVal ty <$> traverse gf e
gf (Cond ty p e e') = Cond ty <$> gf p <*> gf e <*> gf e'
gf (Lam t n e) = Lam t n <$> gf e
gf e@BB{} = pure e; gf e@TB{} = pure e; gf e@UB{} = pure e; gf e@NB{} = pure e
gf e@StrLit{} = pure e; gf e@FLit{} = pure e; gf e@ILit{} = pure e; gf e@BLit{} = pure e
gf e@RC{} = pure e

ug :: IM.IntMap (E (T K)) -> E (T K) -> E (T K)
ug st (Var _ n@(Nm _ (U i) _)) =
    IM.findWithDefault (throw (BadHole n)) i st
ug _ e = e

bsProcess :: RurePtr
          -> Bool -- ^ Flush output?
          -> Int -- ^ Unique context
          -> E (T K)
          -> Either StreamError ([BS.ByteString] -> IO ())
bsProcess _ _ _ AllField{} = Left NakedField
bsProcess _ _ _ Field{}    = Left NakedField
bsProcess _ _ _ (NB _ Ix)  = Left NakedField
bsProcess r f u e | (TyApp _ (TyB _ TyStream) _) <- eLoc e =
    Right (traverse_ g.eStream u r e)
    where g | f = (*>fflush).pDocLn | otherwise = pDocLn
          fflush = hFlush stdout
bsProcess r _ u e =
    Right $ \bs -> pDocLn (eF u r e bs)

pDocLn = putDoc.(<>hardline).pretty


scanM :: Monad m => (b -> a -> m b) -> b -> [a] -> m [b]
scanM op seed xs = sequence $
    scanl' go (pure seed) xs where go seedϵ x = do {seedϵ' <- seedϵ; op seedϵ' x}

eF :: Int -> RurePtr -> E (T K) -> [BS.ByteString] -> E (T K)
eF u r (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) = \bs ->
    let op'=eB u id op; seed'=eB u id seed; xsϵ=eStream u r xs bs
    in evalState (foldM (c2M op') seed' xsϵ) u
eF u r (EApp _ (EApp _ (BB _ Fold1) op) xs) = \bs ->
    let op'=eB u id op; seed':xsϵ=eStream u r xs bs
    in evalState (foldM (c2M op') seed' xsϵ) u
eF u r e = \bs ->
    let (eHoley, (_, folds)) = runState (gf e) (0, [])
        (filledHoles, u') = foldAll u r folds bs
        in eB u' (ug (IM.fromList filledHoles)) eHoley

a1 :: E (T K) -> E (T K) -> UM (E (T K))
a1 f x | TyArr _ _ cod <- eLoc f = lβ (EApp cod f x)

a2 :: E (T K) -> E (T K) -> E (T K) -> UM (E (T K))
a2 op x0 x1 | TyArr _ _ t@(TyArr _ _ t') <- eLoc op = lβ (EApp t' (EApp t op x0) x1)

c1 :: Int -> E (T K) -> E (T K) -> E (T K)
c1 i f x = evalState (eBM id =<< a1 f x) i

c2M op x0 x1 = eBM id =<< a2 op x0 x1
c2Mϵ f g e e' = eBM f =<< a2 g e e'

c2 :: Int -> E (T K) -> E (T K) -> E (T K) -> E (T K)
c2 i op x0 x1 = evalState (c2M op x0 x1) i

eStream :: Int -> RurePtr -> E (T K) -> [BS.ByteString] -> [E (T K)]
eStream u r (EApp _ (EApp _ (EApp _ (TB _ Scan) op) seed) xs) bs =
    let op'=eB u id op; seed'=eB u id seed; xsϵ=eStream u r xs bs
    in evalState (scanM (c2M op') seed' xsϵ) u
eStream i r (EApp _ (UB _ CatMaybes) e) bs = mapMaybe asM$eStream i r e bs
eStream u r (Implicit _ e) bs = zipWith (\fs i -> eB u (eCtx fs i) e) [(b, splitBy r b) | b <- bs] [1..]
eStream _ _ AllColumn{} bs = mkStr<$>bs
eStream _ r (Column _ i) bs = mkStr.(! (i-1)).splitBy r<$>bs
eStream _ r (IParseCol _ n) bs = [parseAsEInt (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (ParseCol (TyApp _ _ (TyB _ TyInteger)) n) bs = [parseAsEInt (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (FParseCol _ n) bs = [parseAsF (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (ParseCol (TyApp _ _ (TyB _ TyFloat)) n) bs = [parseAsF (splitBy r b ! (n-1)) | b <- bs]
eStream i r (EApp _ (EApp _ (BB _ MapMaybe) f) e) bs = let xs = eStream i r e bs in mapMaybe (asM.c1 i f) xs
eStream i r (EApp _ (EApp _ (BB _ Map) f) e) bs = let xs=eStream i r e bs in fmap (c1 i f) xs
eStream i r (EApp _ (EApp _ (BB _ Prior) op) e) bs = let xs=eStream i r e bs in zipWith (c2 i op) (tail xs) xs
eStream i r (EApp _ (EApp _ (BB _ Filter) p) e) bs = let xs=eStream i r e bs; ps=fmap (asB.c1 i p) xs in [x | (pϵ,x) <- zip ps xs, pϵ]
eStream i r (EApp _ (EApp _ (EApp _ (TB _ ZipW) f) e0) e1) bs = let xs0=eStream i r e0 bs; xs1=eStream i r e1 bs in zipWith (c2 i f) xs0 xs1
eStream i r (EApp (TyApp _ _ (TyB _ TyStr)) (UB _ Dedup) e) bs = let s = eStream i r e bs in nubOrdOn asS s
eStream i r (EApp _ (EApp _ (BB _ DedupOn) op) e) bs | TyArr _ _ (TyB _ TyStr) <- eLoc op = let xs = eStream i r e bs in nubOrdOn (asS.c1 i op) xs
eStream i r (EApp _ (EApp _ (BB _ DedupOn) op) e) bs | TyArr _ _ (TyB _ TyInteger) <- eLoc op = let xs = eStream i r e bs in nubOrdOn (asI.c1 i op) xs
eStream i r (EApp _ (EApp _ (BB _ DedupOn) op) e) bs | TyArr _ _ (TyB _ TyFloat) <- eLoc op = let xs = eStream i r e bs in nubOrdOn (asF.c1 i op) xs
eStream u r (Guarded _ p e) bs =
    let bss=(\b -> (b, splitBy r b))<$>bs
    in catMaybes $ zipWith (\fs i -> if asB (eB u (eCtx fs i) p) then Just (eB u (eCtx fs i) e) else Nothing) bss [1..]

asS :: E (T K) -> BS.ByteString
asS (StrLit _ s) = s; asS e = throw (InternalCoercionError e TyStr)

asI :: E (T K) -> Integer
asI (ILit _ i) = i; asI e = throw (InternalCoercionError e TyInteger)

asF :: E (T K) -> Double
asF (FLit _ x) = x; asF e = throw (InternalCoercionError e TyFloat)

asR :: E (T K) -> RurePtr
asR (RC r) = r; asR e = throw (InternalCoercionError e TyR)

asM :: E (T K) -> Maybe (E (T K))
asM (OptionVal _ e) = e; asM e = throw (InternalCoercionError e TyOption)

asB :: E (T K) -> Bool
asB (BLit _ b) = b; asB e = throw (InternalCoercionError e TyBool)

asV :: E (T K) -> V.Vector (E (T K))
asV (Arr _ v) = v; asV e = throw (InternalCoercionError e TyVec)

asT :: E (T K) -> [E (T K)]
asT (Tup _ es) = es; asT e = throw (ExpectedTup e)

eCtx :: (BS.ByteString, V.Vector BS.ByteString) -- ^ Line, split by field separator
     -> Integer -- ^ Line number
     -> E (T K) -> E (T K)
eCtx ~(f, _) _ AllField{}  = mkStr f
eCtx (_, fs) _ (Field _ i) = mkStr (fs ! (i-1))
eCtx (_, fs) _ LastField{} = mkStr (V.last fs)
eCtx _ i (NB _ Ix)         = mkI i
eCtx (_, fs) _ (NB _ Nf)   = mkI (fromIntegral$V.length fs)
eCtx _ _ e                 = e

eB :: Int -> (E (T K) -> E (T K)) -> E (T K) -> E (T K)
eB i f x = evalState (eBM f x) i

{-# SCC eBM #-}
eBM :: (E (T K) -> E (T K)) -> E (T K) -> UM (E (T K))
eBM f (EApp t (EApp _ (EApp _ (TB _ Captures) s) i) r) = do
    s' <- eBM f s; i' <- eBM f i; r' <- eBM f r
    pure $ OptionVal t (mkStr <$> findCapture (asR r') (asS s') (fromIntegral$asI i'))
eBM f (EApp t (EApp _ (EApp _ (TB _ AllCaptures) s) i) r) = do
    s' <- eBM f s; i' <- eBM f i; r' <- eBM f r
    pure $ Arr t (V.fromList (mkStr <$> captures' (asR r') (asS s') (fromIntegral$asI i')))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Max) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (max x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Min) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (min x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Min) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (min x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Max) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (max x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Min) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkStr (min x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Max) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkStr (max x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Plus) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (x0'+x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Minus) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (x0'-x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Times) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (x0'*x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Plus) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'+x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Minus) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'-x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Times) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'*x1'))
eBM f (EApp _ (EApp _ (BB _ Div) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'/x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Eq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Neq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'/=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Gt) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'>x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Lt) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'<x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Leq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'<=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Geq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkB (x0'>=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Gt) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'>x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Lt) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'<x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Eq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Neq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'/=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Geq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'>=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Leq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkB (x0'<=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Gt) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkB (x0'>x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Exp) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (mkI (x0'^x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Exp) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (mkF (x0'**x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Eq) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyApp _ (TyB _ TyVec) t) _) Eq) x0) x1) = do
    x0' <- asV<$>eBM f x0; x1' <- asV<$>eBM f x1
    mkB <$> if V.length x0'==V.length x1'
        then and . fmap asB <$> V.zipWithM (c2Mϵ f op) x0' x1'
        else pure False
    where op = BB (TyArr Star t (TyArr Star t tyB)) Eq
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyApp _ (TyB _ TyOption) t) _) Eq) x0) x1) = do
    x0' <- asM<$>eBM f x0; x1' <- asM<$>eBM f x1
    case (x0',x1') of
        (Nothing, Nothing) -> pure (mkB True)
        (Nothing, Just{})  -> pure (mkB False)
        (Just{}, Nothing)  -> pure (mkB False)
        (Just e0, Just e1) -> c2Mϵ f op e0 e1
    where op = BB (TyArr Star t (TyArr Star t tyB)) Eq
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Plus) x0) x1) = do
    x0' <- asS <$> eBM f x0; x1' <- asS<$>eBM f x1
    pure (mkStr (x0'<>x1'))
eBM f (EApp _ (EApp _ (BB _ And) x0) x1) = do
    x0' <- asB<$>eBM f x0; x1' <- asB<$>eBM f x1
    pure (mkB (x0'&&x1'))
eBM f (EApp _ (EApp _ (BB _ Or) x0) x1) = do
    x0' <- asB<$>eBM f x0; x1' <- asB<$>eBM f x1
    pure (mkB (x0'||x1'))
eBM f (EApp _ (UB _ Not) b) = do {b' <- asB<$>eBM f b; pure $ mkB (not b')}
eBM f (EApp _ (EApp _ (BB _ Matches) s) r) = do
    s' <- asS<$>eBM f s; r' <- asR<$>eBM f r
    pure $ mkB (isMatch' r' s')
eBM f (EApp _ (EApp _ (BB _ NotMatches) s) r) = do
    s' <- asS<$>eBM f s; r' <- asR<$>eBM f r
    pure $ mkB (not$isMatch' r' s')
eBM f (EApp _ (EApp _ (BB _ Split) s) r) = do
    s' <- asS<$>eBM f s; r' <- asR<$>eBM f r
    pure (Arr (tyV tyStr) (mkStr<$>splitBy r' s'))
eBM f (EApp _ (EApp _ (BB _ Splitc) s) c) = do
    s' <- asS<$>eBM f s; c' <- the.asS<$>eBM f c
    pure (Arr (tyV tyStr) (mkStr <$> V.fromList (BS.split c' s')))
eBM f (EApp _ (UB _ FParse) x) = do {x' <- eBM f x; pure (parseAsF (asS x'))}
eBM f (EApp _ (UB _ IParse) x) = do {x' <- eBM f x; pure (parseAsEInt (asS x'))}
eBM f (EApp (TyB _ TyInteger) (UB _ Parse) x) = do {x' <- eBM f x; pure (parseAsEInt (asS x'))}
eBM f (EApp (TyB _ TyFloat) (UB _ Parse) x) = do {x' <- eBM f x; pure (parseAsF (asS x'))}
eBM f (EApp _ (UB _ (At i)) v) = do {v' <- eBM f v; pure (asV v'!(i-1))}
eBM f (EApp _ (UB _ (Select i)) x) = do {x' <- eBM f x; pure (asT x' !! (i-1))}
eBM f (EApp _ (UB _ Floor) x) = do {xr <- asF<$>eBM f x; pure $ mkI (floor xr)}
eBM f (EApp _ (UB _ Ceiling) x) = do {xr <- asF<$>eBM f x; pure $ mkI (ceiling xr)}
eBM f (EApp (TyB _ TyInteger) (UB _ Negate) i) = do {i' <- eBM f i; pure $ mkI (negate (asI i'))}
eBM f (EApp (TyB _ TyFloat) (UB _ Negate) x) = do {x' <- eBM f x; pure $ mkF (negate (asF x'))}
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
eBM f (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) = do
    op' <- eBM f op; seed' <- eBM f seed; xs' <- eBM f xs
    V.foldM (c2Mϵ f op') seed' (asV xs')
eBM f (EApp _ (EApp _ (BB _ Fold1) op) xs) = do
    op' <- eBM f op; xs' <- eBM f xs
    let xsV=asV xs'; Just (seed, xs'') = V.uncons xsV
    V.foldM (c2Mϵ f op') seed xs''
eBM f (EApp yT@(TyApp _ (TyB _ TyOption) _) (EApp _ (BB _ Map) g) x) = do
    g' <- eBM f g; x' <- eBM f x
    let TyArr _ _ cod=eLoc g'
    OptionVal yT <$> traverse (eBM f.EApp cod g') (asM x')
eBM f (EApp yT@(TyApp _ (TyB _ TyVec) _) (EApp _ (BB _ Map) g) x) = do
    g' <- eBM f g; x' <- eBM f x
    let TyArr _ _ cod=eLoc g'
    Arr yT <$> traverse (eBM f.EApp cod g') (asV x')
eBM f (EApp t (EApp _ (EApp _ (TB _ Option) x) g) y) = do
    x' <- eBM f x; g' <- eBM f g; y' <- eBM f y
    case asM y' of
        Nothing -> pure x'
        Just yϵ -> eBM f (EApp t g' yϵ)
eBM f (EApp _ (EApp _ (EApp _ (TB _ Substr) s) i0) i1) = do
    i0' <- eBM f i0; i1' <- eBM f i1; s' <- eBM f s
    pure $ mkStr (substr (asS s') (fromIntegral$asI i0') (fromIntegral$asI i1'))
eBM f (Cond _ p e e') = do {p' <- eBM f p; if asB p' then eBM f e else eBM f e'}
eBM f (Tup t es) = Tup t <$> traverse (eBM f) es
eBM f e = pure (f e)
