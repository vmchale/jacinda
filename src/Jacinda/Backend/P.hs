module Jacinda.Backend.P ( runJac ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad              (foldM)
import           Control.Monad.State.Strict (evalState)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as ASCII
import           Data.Containers.ListUtils  (nubOrdOn)
import           Data.Foldable              (traverse_)
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Vector                as V
import           Data.Word                  (Word8)
import           Jacinda.AST
import           Jacinda.AST.I
import           Jacinda.Backend.Const
import           Jacinda.Backend.Parse
import           Jacinda.Fuse
import           Jacinda.Regex
import           Jacinda.Ty.Const
import           Prettyprinter              (hardline, pretty)
import           Prettyprinter.Render.Text  (putDoc)
import           Regex.Rure                 (RureMatch (RureMatch), RurePtr)

runJac :: RurePtr -- ^ Record separator
       -> Int
       -> Program (T K)
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac re i e = ϝ (bsProcess re (flushD e)) (ϝ fuse $ ib i e) where ϝ = uncurry.flip

data StreamError = NakedField
                 deriving (Show)

instance Exception StreamError where

data EvalError = EmptyFold
               | IndexOutOfBounds Int
               | InternalCoercionError (E (T K)) TB
               deriving (Show)

instance Exception EvalError where

(!) :: V.Vector a -> Int -> a
v ! ix = case v V.!? ix of {Just x  -> x; Nothing -> throw $ IndexOutOfBounds ix}

parseAsEInt :: BS.ByteString -> E (T K)
parseAsEInt = mkI . readDigits

parseAsF :: BS.ByteString -> E (T K)
parseAsF = FloatLit tyF . readFloat

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
    where g | f = undefined | otherwise = putDoc.(<>hardline).pretty
bsProcess r _ u e@(EApp _ (EApp _ (EApp _ (TB _ Fold) _) _) xs) | TyApp _ (TyB _ TyStream) _ <- eLoc xs =
    Right $ \bs -> putDoc (pretty (eF u r e bs) <> hardline)
bsProcess r _ u e@(EApp _ (EApp _ (BB _ Fold1) _) xs) | TyApp _ (TyB _ TyStream) _ <- eLoc xs =
    Right $ \bs -> putDoc (pretty (eF u r e bs) <> hardline)

-- gather folds?
eF :: Int -> RurePtr -> E (T K) -> [BS.ByteString] -> E (T K)
eF u r (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) = \bs ->
    let op'=eB u id op; seed'=eB u id seed; xsϵ=eStream u r xs bs
    in evalState (foldM (applyOp op') seed' xsϵ) u
    where applyOp f e e' = eBM id =<< a2 f e e'
eF u r (EApp _ (EApp _ (BB _ Fold1) op) xs) = \bs ->
    let op'=eB u id op; seed':xsϵ=eStream u r xs bs
    in evalState (foldM (applyOp op') seed' xsϵ) u
    where applyOp f e e' = eBM id =<< a2 f e e'

a1 :: E (T K) -> E (T K) -> UM (E (T K))
a1 f x | TyArr _ _ cod <- eLoc f = lβ (EApp cod f x)

a2 :: E (T K) -> E (T K) -> E (T K) -> UM (E (T K))
a2 op x0 x1 | TyArr _ _ t@(TyArr _ _ t') <- eLoc op = lβ (EApp t' (EApp t op x0) x1)

c1 :: Int -> E (T K) -> E (T K) -> E (T K)
c1 i f x = evalState (eBM id =<< a1 f x) i

c2 :: Int -> E (T K) -> E (T K) -> E (T K) -> E (T K)
c2 i op x0 x1 = evalState (eBM id =<< a2 op x0 x1) i

eStream :: Int -> RurePtr -> E (T K) -> [BS.ByteString] -> [E (T K)]
eStream i r (EApp _ (UB _ CatMaybes) e) bs = mapMaybe asM$eStream i r e bs
eStream u r (Implicit _ e) bs = zipWith (\fs i -> eB u (eCtx fs i) e) [(b, splitBy r b) | b <- bs] [1..]
eStream _ _ AllColumn{} bs = mkStr<$>bs
eStream _ r (IParseCol _ n) bs = [parseAsEInt (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (ParseCol (TyApp _ _ (TyB _ TyInteger)) n) bs = [parseAsEInt (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (FParseCol _ n) bs = [parseAsF (splitBy r b ! (n-1)) | b <- bs]
eStream _ r (ParseCol (TyApp _ _ (TyB _ TyFloat)) n) bs = [parseAsF (splitBy r b ! (n-1)) | b <- bs]
eStream i r (EApp _ (EApp _ (BB _ MapMaybe) f) e) bs = let xs = eStream i r e bs in mapMaybe (asM.c1 i f) xs
eStream i r (EApp _ (EApp _ (BB _ Map) f) e) bs = let xs=eStream i r e bs in fmap (c1 i f) xs
eStream i r (EApp _ (EApp _ (BB _ Prior) op) e) bs = let xs=eStream i r e bs in zipWith (c2 i op) (tail xs) xs
eStream i r (EApp _ (EApp _ (BB _ Filter) p) e) bs = let xs=eStream i r e bs; ps=fmap (asB.c1 i p) xs in [x | (pϵ,x) <- zip ps xs, pϵ]
eStream i r (EApp (TyApp _ _ (TyB _ TyStr)) (UB _ Dedup) e) bs = let s = eStream i r e bs in nubOrdOn asS s
eStream i r (EApp _ (EApp _ (BB _ DedupOn) op) e) bs | TyArr _ _ (TyB _ TyStr) <- eLoc op = let xs = eStream i r e bs in nubOrdOn (asS.c1 i op) xs
eStream u r (Guarded _ p e) bs = let bss=(\b -> (b, splitBy r b))<$>bs in catMaybes$zipWith (\fs i -> if asB (eB u (eCtx fs i) p) then Just (eB u (eCtx fs i) e) else Nothing) bss [1..]

asS :: E (T K) -> BS.ByteString
asS (StrLit _ s) = s; asS e = throw (InternalCoercionError e TyStr)

asI :: E (T K) -> Integer
asI (IntLit _ i) = i; asI e = throw (InternalCoercionError e TyInteger)

asF :: E (T K) -> Double
asF (FloatLit _ x) = x; asF e = throw (InternalCoercionError e TyFloat)

asR :: E (T K) -> RurePtr
asR (RegexCompiled r) = r; asR e = throw (InternalCoercionError e TyR)

asM :: E (T K) -> Maybe (E (T K))
asM (OptionVal _ e) = e; asM e = throw (InternalCoercionError e TyOption)

asB :: E (T K) -> Bool
asB (BoolLit _ b) = b; asB e = throw (InternalCoercionError e TyBool)

asV :: E (T K) -> V.Vector (E (T K))
asV (Arr _ v) = v; asV e = throw (InternalCoercionError e TyVec)

eCtx :: (BS.ByteString, V.Vector BS.ByteString) -- ^ Line, split by field separator
     -> Integer -- ^ Line number
     -> E (T K) -> E (T K)
eCtx ~(f, _) _ AllField{}  = mkStr f
eCtx (_, fs) _ (Field _ i) = mkStr (fs ! (i-1))
eCtx (_, fs) _ LastField{} = mkStr (V.last fs)
eCtx _ i (NB _ Ix)         = mkI i
eCtx _ _ e                 = e

eB :: Int -> (E (T K) -> E (T K)) -> E (T K) -> E (T K)
eB i f x = evalState (eBM f x) i

eBM :: (E (T K) -> E (T K)) -> E (T K) -> UM (E (T K))
eBM f (EApp _ (EApp _ (EApp _ (TB _ Captures) s) i) r) = do
    r' <- eBM f r; s' <- eBM f s; i' <- eBM f i
    let mRes = findCapture (asR r') (asS s') (fromIntegral (asI i'))
    pure $ OptionVal (TyApp undefined (TyB undefined TyOption) (TyB undefined TyStr)) (fmap mkStr mRes)
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Max) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkI (max x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Min) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkI (min x0' x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Plus) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkI (x0'+x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Minus) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkI (x0'-x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Times) x0) x1) = do
    x0' <- asI <$> eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkI (x0'*x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Plus) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkF (x0'+x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Minus) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkF (x0'-x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Times) x0) x1) = do
    x0' <- asF <$> eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkF (x0'*x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Eq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Neq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'/=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Gt) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'>x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Lt) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'<x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Leq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'<=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Geq) x0) x1) = do
    x0' <- asI<$>eBM f x0; x1' <- asI<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'>=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Gt) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'>x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Lt) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'<x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Eq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Neq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'/=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Geq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'>=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyFloat) _) Leq) x0) x1) = do
    x0' <- asF<$>eBM f x0; x1' <- asF<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'<=x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Eq) x0) x1) = do
    x0' <- asS<$>eBM f x0; x1' <- asS<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'==x1'))
eBM f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Plus) x0) x1) = do
    x0' <- asS <$> eBM f x0; x1' <- asS<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkStr (x0'<>x1'))
eBM f (EApp _ (EApp _ (BB _ And) x0) x1) = do
    x0' <- asB<$>eBM f x0; x1' <- asB<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'&&x1'))
eBM f (EApp _ (EApp _ (BB _ Or) x0) x1) = do
    x0' <- asB<$>eBM f x0; x1' <- asB<$>eBM f x1
    pure (x0' `seq` x1' `seq` mkB (x0'||x1'))
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
eBM f (EApp _ (UB _ Tally) e) = do
    s' <- eBM f e
    let r =fromIntegral (BS.length$asS s')
    pure (r `seq` mkI r)
eBM f (EApp _ (EApp _ (UB _ Const) e) _) = eBM f e
eBM f (EApp _ (EApp _ (BB _ Fold1) op) xs) = do
    op' <- eBM f op; xs' <- eBM f xs
    let xsV=asV xs'; Just (seed, xs'') = V.uncons xsV
    V.foldM (applyOp op') seed xs''
    where applyOp g e e' = eBM f =<< a2 g e e'
eBM f e = pure (f e)
