{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Regex ( splitBy
                     , splitWhitespace
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

-- there's some funny business going on in this module with unsafePerformIO but
-- it works because Rust doesn't seem to modify the compiled thing, just read
-- it.
--
-- anyway this could in theory go awry if something got allocated in the same
-- pointer or whatever but... I don't think that'll happen.

-- FIXME: compile can be "pure', mkIter can not!!
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
isMatch' re haystack = unsafePerformIO $ isMatch re haystack 0

compileDefault :: BS.ByteString -> RurePtr
compileDefault = unsafePerformIO . (yeetRureIO <=< compile rureDefaultFlags) -- TODO: rureFlagDotNL? in case they have weird records

newtype RureExe = RegexCompile String deriving (Show)

instance Exception RureExe where

yeetRureIO :: Either String a -> IO a
yeetRureIO = either (throwIO . RegexCompile) pure
