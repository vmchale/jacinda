module Jacinda.Backend.P ( runJac ) where

import           Control.Exception     (Exception, throw)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as ASCII
import qualified Data.Text             as T
import           Data.Text.Encoding    (decodeUtf8)
import qualified Data.Vector           as V
import           Jacinda.AST
import           Jacinda.AST.I
import           Jacinda.Backend.Const
import           Jacinda.Backend.Parse
import           Jacinda.Fuse
import           Jacinda.Regex
import           Jacinda.Ty.Const
import           Regex.Rure            (RurePtr)

runJac :: RurePtr -- ^ Record separator
       -> Int
       -> Program (T K)
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac re i e = ϝ (bsProcess re (flushD e)) $ (ϝ fuse $ ib i e) where ϝ = uncurry.flip

data StreamError = NakedField
                 deriving (Show)

instance Exception StreamError where

data EvalError = EmptyFold
               | IndexOutOfBounds Int
               | InternalCoercionError TB
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

-- BETA REDUCTION CONTEXT?? without causing failure-to-stream
-- also maybe need to pass forward an int-unique to fold (later) I dunno
eStream :: RurePtr -> E (T K) -> [BS.ByteString] -> [E (T K)]
eStream r (EApp _ (UB _ CatMaybes) e) bs = undefined
eStream r (Implicit _ e) bs              = zipWith (\fs i -> eCtx fs i e) [splitBy r b | b <- bs] [1..]

asS :: E (T K) -> BS.ByteString
asS (StrLit _ s) = s; asS _ = throw (InternalCoercionError TyStr)

asI :: E (T K) -> Integer
asI (IntLit _ i) = i; asI _ = throw (InternalCoercionError TyInteger)

asR :: E (T K) -> RurePtr
asR (RegexCompiled r) = r; asR _ = throw (InternalCoercionError TyR)

asM :: E (T K) -> Maybe (E (T K))
asM (OptionVal _ e) = e; asM _ = throw (InternalCoercionError TyOption)

-- .?{|`1 ~* 1 /([^\?]*)/}
eCtx :: V.Vector BS.ByteString -- ^ Line, split by field separator
     -> Integer -- ^ Line number
     -> E (T K) -> E (T K)
eCtx fs _ (Field _ i) = mkStr (fs ! (i-1))
eCtx _ i (NB _ Ix)    = mkI i
eCtx fs i e           = eStep (eCtx fs i) e

eStep :: (E (T K) -> E (T K)) -> E (T K) -> E (T K)
eStep f (EApp _ (EApp _ (EApp _ (TB _ Captures) s) i) r) =
    let mRes = findCapture (asR$eStep f r) (asS$eStep f s) (fromIntegral$asI$eStep f i)
    in OptionVal (TyApp undefined (TyB undefined TyOption) (TyB undefined TyStr)) (fmap mkStr mRes)

atField :: RurePtr
        -> Int
        -> BS.ByteString -- ^ Line
        -> BS.ByteString
atField re i = (! (i-1)) . splitBy re
