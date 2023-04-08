module Jacinda.Backend.P ( runJac ) where

import qualified Data.ByteString        as BS
import           Jacinda.AST
import           Jacinda.AST.I
import Control.Exception (Exception, throw)
import Jacinda.Backend.Parse
import qualified Data.Vector as V
import Jacinda.Backend.Const
import           Jacinda.Fuse
import Jacinda.Regex
import Jacinda.Ty.Const
import Regex.Rure (RurePtr)
import qualified Data.ByteString.Char8 as ASCII

runJac :: RurePtr -- ^ Record separator
       -> Int
       -> Program (T K)
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac re i e = bsProcess re (flushD e) (fst $ uncurry (flip fuse) $ ib i e)

data StreamError = NakedField
                 deriving (Show)

instance Exception StreamError where

data EvalError = EmptyFold
               | IndexOutOfBounds Int
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
          -> E (T K)
          -> Either StreamError ([BS.ByteString] -> IO ())
bsProcess _ _ AllField{} = Left NakedField
bsProcess _ _ Field{}    = Left NakedField
bsProcess _ _ (NB _ Ix)  = Left NakedField

atField :: RurePtr
        -> Int
        -> BS.ByteString -- ^ Line
        -> BS.ByteString
atField re i = (! (i-1)) . splitBy re

ir :: RurePtr
   -> E (T K)
   -> [BS.ByteString]
   -> [E (T K)]
ir _ AllColumn{} = fmap mkStr
ir re (Column _ i) = fmap (mkStr . atField re i)
ir re (IParseCol _ i) = fmap (parseAsEInt . atField re i)
ir re (FParseCol _ i) = fmap (parseAsF . atField re i)
ir re (ParseCol ty@(TyApp _ _ (TyB _ TyFloat)) i) = ir re (FParseCol ty i)
ir re (ParseCol ty@(TyApp _ _ (TyB _ TyInteger)) i) = ir re (IParseCol ty i)