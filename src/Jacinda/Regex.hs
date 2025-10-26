{-# LANGUAGE OverloadedLists #-}

module Jacinda.Regex ( lazySplit
                     , lazySplitH
                     , splitBy
                     , splitH
                     , defaultRurePtr
                     , isMatch'
                     , find'
                     , sub1, subs
                     , compileDefault
                     , substr
                     , findCapture
                     , captures'
                     , capturesIx
                     ) where

import           Control.Exception        (Exception, throwIO)
import           Control.Monad            ((<=<))
import           Data.Bifunctor           (bimap, first)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy     as BSL
import qualified Data.Vector              as V
import qualified DL
import           Foreign.C.Types          (CSize)
import           Foreign.ForeignPtr       (plusForeignPtr)
import           Regex.Rure               (RureFlags, RureMatch (..), RurePtr, captures, compile, find, findCaptures, isMatch, matches', rureDefaultFlags, rureFlagDotNL)
import           System.IO.Unsafe         (unsafeDupablePerformIO, unsafePerformIO)

-- https://docs.rs/regex/latest/regex/#perl-character-classes-unicode-friendly
defaultFs :: BS.ByteString
defaultFs = "\\s+"

{-# NOINLINE defaultRurePtr #-}
defaultRurePtr :: RurePtr
defaultRurePtr = unsafePerformIO $ yIO =<< compile genFlags defaultFs

genFlags :: RureFlags
genFlags = rureDefaultFlags <> rureFlagDotNL

substr :: BS.ByteString -> Int -> Int -> BS.ByteString
substr (BS.BS fp l) begin endϵ | endϵ >= begin = BS.BS (fp `plusForeignPtr` begin) (min l endϵ-begin)
                               | otherwise = "error: invalid substring indices."

captures' :: RurePtr -> BS.ByteString -> CSize -> [BS.ByteString]
captures' re haystack@(BS.BS fp _) ix = unsafeDupablePerformIO $ fmap go <$> captures re haystack ix
    where go (RureMatch s e) =
            let e' = fromIntegral e; s' = fromIntegral s
            in BS.BS (fp `plusForeignPtr` s') (e'-s')

{-# NOINLINE capturesIx #-}
capturesIx :: RurePtr -> BS.ByteString -> CSize -> [RureMatch]
capturesIx re str n = unsafeDupablePerformIO $ captures re str n

{-# NOINLINE findCapture #-}
findCapture :: RurePtr -> BS.ByteString -> CSize -> Maybe BS.ByteString
findCapture re haystack@(BS.BS fp _) ix = unsafeDupablePerformIO $ fmap go <$> findCaptures re haystack ix 0
    where go (RureMatch s e) =
            let e' = fromIntegral e
                s' = fromIntegral s
                in BS.BS (fp `plusForeignPtr` s') (e'-s')

{-# NOINLINE subs #-}
subs :: RurePtr -> BS.ByteString -> BS.ByteString -> BS.ByteString
subs re haystack = let ms = unsafeDupablePerformIO $ matches' re haystack in go Nothing ms
    where go _ [] _                                         = haystack
          go (Just (RureMatch _ pe)) ((RureMatch ms _):_) _ | pe > ms = error "Overlapping matches."
          go _ (m@(RureMatch ms me):s) substituend          = let next=go (Just m) s substituend in BS.take (fromIntegral ms) next <> substituend <> BS.drop (fromIntegral me) next

sub1 :: RurePtr -> BS.ByteString -> BS.ByteString -> BS.ByteString
sub1 re bs ss =
    case find' re bs of
        Nothing              -> bs
        Just (RureMatch s e) -> BS.take (fromIntegral s) bs <> ss <> BS.drop (fromIntegral e) bs

{-# NOINLINE find' #-}
find' :: RurePtr -> BS.ByteString -> Maybe RureMatch
find' re str = unsafeDupablePerformIO $ find re str 0

lazySplitH :: RurePtr -> BSL.ByteString -> [BS.ByteString]
lazySplitH rp = DL.toList . go Nothing . BSL.toChunks where
    go Nothing [] = DL.empty
    go Nothing (c:cs) =
        case splitH rp c of
            Just (iss,lss) -> iss<>go (Just lss) cs
            Nothing        -> go Nothing cs
    go (Just c) [] = splitHLast rp c
    go (Just e) (c:cs) =
        case splitH rp (e<>c) of
            Just (iss,lss) -> iss<>go (Just lss) cs
            Nothing        -> go Nothing cs

lazySplit :: RurePtr -> BSL.ByteString -> [BS.ByteString]
lazySplit rp = DL.toList . go Nothing . BSL.toChunks where
    go Nothing []      = DL.empty
    go Nothing (c:cs)  =
        case splitByDL rp c of
            Just (iss,lss) -> iss<>go (Just lss) cs
            Nothing        -> go Nothing cs
    go (Just c) []     = splitByL rp c
    go (Just e) (c:cs) =
        case splitByDL rp (e<>c) of
            Just (iss,lss) -> iss<>go (Just lss) cs
            Nothing        -> go Nothing cs

{-# NOINLINE splitByL #-}
splitByL :: RurePtr -> BS.ByteString -> DL.DL BS.ByteString
splitByL rp b = case splitByDL rp b of {Nothing -> DL.empty; Just (as,a) -> as `DL.snoc` a}

{-# NOINLINE splitBy #-}
splitBy :: RurePtr -> BS.ByteString -> V.Vector BS.ByteString
splitBy _ "" = []
splitBy re haystack@(BS.BS fp l) =
    V.fromList [ BS.BS (fp `plusForeignPtr` s) (e-s) | (s,e) <- slicePairs ]
    where ixes = unsafeDupablePerformIO $ matches' re haystack
          slicePairs = case ixes of
                (RureMatch 0 i:rms) -> mkMiddle (fromIntegral i) rms
                rms                 -> mkMiddle 0 rms
          mkMiddle begin' []        = [(begin', l)]
          mkMiddle begin' (rm0:rms) = (begin', fromIntegral (start rm0)) : mkMiddle (fromIntegral $ end rm0) rms

{-# SCC splitByDL #-}
{-# NOINLINE splitByDL #-}
splitByDL :: RurePtr -> BS.ByteString
          -> Maybe (DL.DL BS.ByteString, BS.ByteString)
splitByDL _ "" = Nothing
splitByDL re haystack@(BS.BS fp l) = bimap (fmap pp) pp <$> slicePairs
    where ixes = unsafeDupablePerformIO $ matches' re haystack
          slicePairs = case ixes of
                (RureMatch 0 i:rms) -> mkMiddle (fromIntegral i) rms
                rms                 -> mkMiddle 0 rms
          mkMiddle begin' []        = Just (DL.empty, (begin', l))
          mkMiddle begin' (rm0:rms) = first ((begin', fromIntegral (start rm0)) `DL.cons`) <$> mkMiddle (fromIntegral $ end rm0) rms
          pp (s,e) = BS.BS (fp `plusForeignPtr` s) (e-s)

{-# NOINLINE splitHLast #-}
splitHLast :: RurePtr -> BS.ByteString -> DL.DL BS.ByteString
splitHLast rp b = case splitH rp b of {Nothing -> DL.empty; Just (as,a) -> as `DL.snoc` a}

{-# NOINLINE splitH #-}
splitH :: RurePtr -> BS.ByteString -> Maybe (DL.DL BS.ByteString, BS.ByteString)
splitH _ "" = Nothing
splitH re haystack@(BS.BS fp l) = bimap (fmap pp) pp <$> chopAt 0 ixes
    where ixes = unsafeDupablePerformIO $ matches' re haystack
          chopAt begin []                  = Just (DL.empty, (begin, l))
          chopAt begin (RureMatch b _:rms) = let b'=fromIntegral b in first ((begin, b') `DL.cons`) <$> chopAt b' rms
          pp (s,e) = BS.BS (fp `plusForeignPtr` s) (e-s)

isMatch' :: RurePtr -> BS.ByteString -> Bool
isMatch' re haystack = unsafeDupablePerformIO $ isMatch re haystack 0

compileDefault :: BS.ByteString -> RurePtr
compileDefault = unsafeDupablePerformIO . (yIO <=< compile genFlags)

newtype RureExe = RegexCompile String

instance Show RureExe where show (RegexCompile str) = str
instance Exception RureExe where

yIO :: Either String a -> IO a
yIO = either (throwIO . RegexCompile) pure
