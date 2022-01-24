{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Regex ( splitBy
                     , splitWhitespace
                     , defaultRurePtr
                     , isMatch'
                     , find'
                     , compileDefault
                     , substr
                     , findCapture
                     ) where

import           Control.Exception        (Exception, throwIO)
import           Control.Monad            ((<=<))
import qualified Data.ByteString.Internal as BS
import           Data.Semigroup           ((<>))
import qualified Data.Vector              as V
import           Foreign.C.Types          (CSize)
import           Foreign.ForeignPtr       (plusForeignPtr)
import           Regex.Rure               (RureMatch (..), RurePtr, captures, compile, find, findCaptures, isMatch, matches', rureDefaultFlags, rureFlagDotNL)
import           System.IO.Unsafe         (unsafeDupablePerformIO, unsafePerformIO)

-- see: https://docs.rs/regex/latest/regex/#perl-character-classes-unicode-friendly
defaultFs :: BS.ByteString
defaultFs = "\\s+"

{-# NOINLINE defaultRurePtr #-}
defaultRurePtr :: RurePtr
defaultRurePtr = unsafePerformIO $ yeetRureIO =<< compile genFlags defaultFs
    where genFlags = rureDefaultFlags <> rureFlagDotNL -- in case they want to use a weird custom record separator

splitWhitespace :: BS.ByteString -> V.Vector BS.ByteString
splitWhitespace = splitBy defaultRurePtr

substr :: BS.ByteString -> Int -> Int -> BS.ByteString
substr (BS.BS fp l) begin endϵ | endϵ >= begin = BS.BS (fp `plusForeignPtr` begin) (min l endϵ - begin)
                               | otherwise = "error: invalid substring indices."

{-# NOINLINE findCapture #-}
findCapture :: RurePtr -> BS.ByteString -> CSize -> Maybe RureMatch
findCapture re haystack ix = unsafeDupablePerformIO $ findCaptures re haystack ix 0

{-# NOINLINE find' #-}
find' :: RurePtr -> BS.ByteString -> Maybe RureMatch
find' re str = unsafeDupablePerformIO $ find re str 0

{-# NOINLINE splitBy #-}
splitBy :: RurePtr
        -> BS.ByteString
        -> V.Vector BS.ByteString
splitBy re haystack@(BS.BS fp l) =
    (\sp -> V.fromList [BS.BS (fp `plusForeignPtr` s) (e-s) | (s, e) <- sp]) slicePairs
    where ixes = unsafeDupablePerformIO $ matches' re haystack
          slicePairs = case ixes of
                (RureMatch 0 i:rms) -> mkMiddle (fromIntegral i) rms
                rms                 -> mkMiddle 0 rms
          mkMiddle begin' []        = [(begin', l)]
          mkMiddle begin' (rm0:rms) = (begin', fromIntegral (start rm0)) : mkMiddle (fromIntegral $ end rm0) rms

isMatch' :: RurePtr
         -> BS.ByteString
         -> Bool
isMatch' re haystack = unsafeDupablePerformIO $ isMatch re haystack 0

compileDefault :: BS.ByteString -> RurePtr
compileDefault = unsafeDupablePerformIO . (yeetRureIO <=< compile rureDefaultFlags) -- TODO: rureFlagDotNL? in case they have weird records

newtype RureExe = RegexCompile String deriving (Show)

instance Exception RureExe where

yeetRureIO :: Either String a -> IO a
yeetRureIO = either (throwIO . RegexCompile) pure
