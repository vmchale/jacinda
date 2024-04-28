module Jacinda.Backend.Parse ( readDigits, readFloat ) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as ASCII
import           Foreign.C.String      (CString)
import           System.IO.Unsafe      (unsafeDupablePerformIO)

readFloat :: BS.ByteString -> Double
readFloat = unsafeDupablePerformIO . (`BS.useAsCString` atof)

foreign import ccall unsafe atof :: CString -> IO Double

readDigits :: BS.ByteString -> Integer
readDigits b | Just (45, bs) <- BS.uncons b = negate $ readDigits bs
readDigits b = ASCII.foldl' (\seed x -> 10 * seed + f x) 0 b
    where f '0' = 0; f '1' = 1; f '2' = 2; f '3' = 3;
          f '4' = 4; f '5' = 5; f '6' = 6; f '7' = 7;
          f '8' = 8; f '9' = 9
          f c   = error (c:" is not a valid digit!")
