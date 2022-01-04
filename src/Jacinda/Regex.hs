{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Regex ( splitBy
                     , splitWhitespace
                     , defaultRurePtr
                     , isMatch'
                     , compileDefault
                     ) where

import           Control.Exception        (Exception, throwIO)
import           Control.Monad            ((<=<))
import qualified Data.ByteString.Internal as BS
import           Data.Semigroup           ((<>))
import qualified Data.Vector              as V
import           Foreign.ForeignPtr       (plusForeignPtr)
import           Regex.Rure               (RureMatch (..), RurePtr, compile, isMatch, matches, mkIter, rureDefaultFlags, rureFlagDotNL)
import           System.IO.Unsafe         (unsafeDupablePerformIO, unsafePerformIO)

-- see: https://docs.rs/regex/latest/regex/#perl-character-classes-unicode-friendly
defaultFs :: BS.ByteString
defaultFs = "\\s+"

-- also ls -l | ja '{ix>1}{`5:i}'

{-# NOINLINE defaultRurePtr #-}
defaultRurePtr :: RurePtr
defaultRurePtr = unsafePerformIO $ yeetRureIO =<< compile genFlags defaultFs
    where genFlags = rureDefaultFlags <> rureFlagDotNL -- in case they want to use a weird custom record separator

splitWhitespace :: BS.ByteString -> V.Vector BS.ByteString
splitWhitespace = splitBy defaultRurePtr

{-# NOINLINE splitBy #-}
splitBy :: RurePtr
        -> BS.ByteString
        -> V.Vector BS.ByteString
splitBy re haystack@(BS.BS fp l) =
    (\sp -> V.fromList [BS.BS (fp `plusForeignPtr` s) (e-s) | (s, e) <- sp]) slicePairs
    where ixes = unsafeDupablePerformIO $ do { reIptr <- mkIter re; matches reIptr haystack }
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
