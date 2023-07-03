module Jacinda.Backend.P ( runJac ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad              (foldM)
import           Control.Monad.State.Strict (evalState)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as ASCII
import           Data.Containers.ListUtils  (nubOrdOn)
import           Data.Foldable              (foldl', traverse_)
import           Data.Maybe                 (catMaybes, mapMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Vector                as V
import           Jacinda.AST
import           Jacinda.AST.I
import           Jacinda.Backend.Const
import           Jacinda.Backend.Parse
import           Jacinda.Fuse
import           Jacinda.Regex
import           Jacinda.Ty.Const
import           Prettyprinter              (hardline, pretty)
import           Prettyprinter.Render.Text  (putDoc)
import           Regex.Rure                 (RurePtr)

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
readFloat = read . ASCII.unpack -- TODO: readMaybe

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
    Right $ \bs -> putDoc (pretty (eF u r e bs))
bsProcess r _ u e@(EApp _ (EApp _ (BB _ Fold1) _) xs) | TyApp _ (TyB _ TyStream) _ <- eLoc xs =
    Right $ \bs -> putDoc (pretty (eF u r e bs))

-- gather folds?
eF :: Int -> RurePtr -> E (T K) -> [BS.ByteString] -> E (T K)
eF u r (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) = \bs ->
    let op'=eB id op; seed'=eB id seed; xsϵ=eStream u r xs bs
    in evalState (foldM (applyOp op') seed' xsϵ) u
    where applyOp f e e' = eB id<$>lβ (EApp undefined (EApp undefined f e) e')
eF u r (EApp _ (EApp _ (BB _ Fold1) op) xs) = \bs ->
    let op'=eB id op; seed':xsϵ=eStream u r xs bs
    in evalState (foldM (applyOp op') seed' xsϵ) u
    where applyOp f e e' = eB id<$>lβ (EApp undefined (EApp undefined f e) e')

eStream :: Int -> RurePtr -> E (T K) -> [BS.ByteString] -> [E (T K)]
eStream i r (EApp _ (UB _ CatMaybes) e) bs = mapMaybe asM$eStream i r e bs
eStream _ r (Implicit _ e) bs = zipWith (\fs i -> eB (eCtx fs i) e) [(b, splitBy r b) | b <- bs] [1..]
eStream _ _ AllColumn{} bs = mkStr<$>bs
eStream i r (EApp _ (EApp _ (BB _ MapMaybe) f) e) bs = let xs = eStream i r e bs in mapMaybe (\eϵ -> asM (eB id$mapOp i f eϵ)) xs
eStream i r (EApp (TyApp _ _ (TyB _ TyStr)) (UB _ Dedup) e) bs = let s = eStream i r e bs in nubOrdOn asS s
eStream i r (EApp _ (EApp _ (BB _ DedupOn) op) e) bs | TyArr _ _ (TyB _ TyStr) <- eLoc op = let xs = eStream i r e bs in nubOrdOn (\eϵ -> asS$eB id$mapOp i op eϵ) xs
eStream _ r (Guarded _ p e) bs = let bss=(\b -> (b, splitBy r b))<$>bs in catMaybes$zipWith (\fs i -> if asB (eB (eCtx fs i) p) then Just (eB (eCtx fs i) e) else Nothing) bss [1..]

mapOp :: Int -> E (T K) -> E (T K) -> E (T K)
mapOp i f x | TyArr _ _ cod <- eLoc f = fst (β i (EApp cod f x))

asS :: E (T K) -> BS.ByteString
asS (StrLit _ s) = s; asS e = throw (InternalCoercionError e TyStr)

asI :: E (T K) -> Integer
asI (IntLit _ i) = i; asI e = throw (InternalCoercionError e TyInteger)

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

eB :: (E (T K) -> E (T K)) -> E (T K) -> E (T K)
eB f (EApp _ (EApp _ (EApp _ (TB _ Captures) s) i) r) =
    let mRes = findCapture (asR$eB f r) (asS$eB f s) (fromIntegral$asI$eB f i)
    in OptionVal (TyApp undefined (TyB undefined TyOption) (TyB undefined TyStr)) (fmap mkStr mRes)
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Plus) x0) x1) =
    let x0'=asI(eB f x0); x1'=asI(eB f x1)
    in mkI (x0'+x1')
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Eq) x0) x1) =
    let x0'=asI(eB f x0); x1'=asI(eB f x1)
    in mkB (x0'==x1')
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Gt) x0) x1) =
    let x0'=asI(eB f x0); x1'=asI(eB f x1)
    in mkB (x0'>x1')
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Geq) x0) x1) =
    let x0'=asI(eB f x0); x1'=asI(eB f x1)
    in mkB (x0'>=x1')
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Leq) x0) x1) =
    let x0'=asI(eB f x0); x1'=asI(eB f x1)
    in mkB (x0'<=x1')
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Lt) x0) x1) =
    let x0'=asI(eB f x0); x1'=asI(eB f x1)
    in mkB (x0'<x1')
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Neq) x0) x1) =
    let x0'=asI(eB f x0); x1'=asI(eB f x1)
    in mkB (x0'/=x1')
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Eq) x0) x1) =
    let x0'=asS(eB f x0); x1'=asS(eB f x1)
    in mkB (x0'==x1')
eB f (EApp _ (EApp _ (BB _ Matches) s) r) =
    let s'=asS(eB f s); r'=asR(eB f r)
    in mkB (isMatch' r' s')
eB f (EApp _ (EApp _ (BB _ NotMatches) s) r) =
    let s'=asS(eB f s); r'=asR(eB f r)
    in mkB (not$isMatch' r' s')
eB f (EApp _ (EApp _ (BB _ Split) s) r) =
    let s'=asS(eB f s); r'=asR(eB f r)
    in Arr (tyV tyStr) (mkStr<$>splitBy r' s')
eB f (EApp _ (UB _ (At i)) v) =
    let v'=asV(eB f v)
    in v'!(i-1)
eB f (EApp _ (EApp _ (UB _ Const) e) _) = eB f e
eB f e = f e
