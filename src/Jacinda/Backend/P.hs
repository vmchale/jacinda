module Jacinda.Backend.P ( runJac ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad.State.Strict (evalState)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as ASCII
import           Data.Containers.ListUtils  (nubOrd)
import           Data.Foldable              (traverse_)
import           Data.Maybe                 (catMaybes, mapMaybe)
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

eStream :: Int -> RurePtr -> E (T K) -> [BS.ByteString] -> [E (T K)]
eStream i r (EApp _ (UB _ CatMaybes) e) bs                                    = mapMaybe asM$eStream i r e bs
eStream _ r (Implicit _ e) bs                                                 = zipWith (\fs i -> eB (eCtx fs i) e) [(b, splitBy r b) | b <- bs] [1..]
eStream _ _ AllColumn{} bs                                                    = mkStr<$>bs
eStream i r (EApp _ (EApp _ (BB _ MapMaybe) f) e) bs                          = let xs = eStream i r e bs in mapMaybe (\eϵ -> asM (eB id$mapOp i f eϵ)) xs
eStream i r (EApp (TyApp _ (TyB _ TyStream) (TyB _ TyStr)) (UB _ Dedup) e) bs = let s = eStream i r e bs in mkStr<$>nubOrd(asS<$>s)
eStream _ r (Guarded _ p e) bs                                                = let bss=(\b -> (b, splitBy r b))<$>bs in catMaybes$zipWith (\fs i -> if asB (eB (eCtx fs i) p) then Just (eB (eCtx fs i) e) else Nothing) bss [1..]

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

eCtx :: (BS.ByteString, V.Vector BS.ByteString) -- ^ Line, split by field separator
     -> Integer -- ^ Line number
     -> E (T K) -> E (T K)
eCtx ~(f, _) _ AllField{}  = mkStr f
eCtx (_, fs) _ (Field _ i) = mkStr (fs ! (i-1))
eCtx _ i (NB _ Ix)         = mkI i
eCtx _ _ e                 = e

eB :: (E (T K) -> E (T K)) -> E (T K) -> E (T K)
eB f (EApp _ (EApp _ (EApp _ (TB _ Captures) s) i) r) =
    let mRes = findCapture (asR$eB f r) (asS$eB f s) (fromIntegral$asI$eB f i)
    in OptionVal (TyApp undefined (TyB undefined TyOption) (TyB undefined TyStr)) (fmap mkStr mRes)
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyInteger) _) Eq) x0) x1) =
    let x0'=asI(eB f x0); x1'=asI(eB f x1)
    in mkB (x0'==x1')
eB f (EApp _ (EApp _ (BB (TyArr _ (TyB _ TyStr) _) Eq) x0) x1) =
    let x0'=asS(eB f x0); x1'=asS(eB f x1)
    in mkB (x0'==x1')
eB f (EApp _ (EApp _ (BB _ Matches) s) r) =
    let s'=asS(eB f s); r'=asR(eB f r)
    in mkB (isMatch' r' s')
eB f (EApp _ (EApp _ (BB _ NotMatches) s) r) =
    let s'=asS(eB f s); r'=asR(eB f r)
    in mkB (not$isMatch' r' s')
eB f e = f e
