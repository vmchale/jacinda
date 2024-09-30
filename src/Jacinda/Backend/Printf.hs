{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Backend.Printf ( sprintf
                              ) where

import           A
import qualified Data.ByteString                   as BS
import           Data.ByteString.Builder           (toLazyByteString)
import           Data.ByteString.Builder.RealFloat (doubleDec)
import qualified Data.ByteString.Lazy              as BSL
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)

sprintf :: BS.ByteString -- ^ Format string
        -> E a
        -> BS.ByteString
sprintf fmt e = encodeUtf8 (sprintf' (decodeUtf8 fmt) e)

pf :: Double -> T.Text
pf = decodeUtf8 . BSL.toStrict . toLazyByteString . doubleDec

-- TODO: interpret precision, like %0.6f %.6

-- FIXME: if next is, say %i and encounter an int in the tuple, that should be an error
sprintf' :: T.Text -> E a -> T.Text
sprintf' fmt (Lit _ (FLit f)) =
    let (prefix, fmt') = T.breakOn "%f" fmt
        in prefix <> pf f <> T.drop 2 fmt'
sprintf' fmt (Lit _ (ILit i)) =
    let (prefix, fmt') = T.breakOn "%i" fmt
        in prefix <> T.pack (show i) <> T.drop 2 fmt'
sprintf' fmt (Lit _ (StrLit bs)) =
    let (prefix, fmt') = T.breakOn "%s" fmt
        in prefix <> decodeUtf8 bs <> T.drop 2 fmt'
sprintf' fmt (Tup _ [e]) = sprintf' fmt e
sprintf' fmt (Tup l (e:es)) =
    let nextFmt = sprintf' fmt e
        in sprintf' nextFmt (Tup l es)
sprintf' fmt (Lit _ (BLit b)) =
    let (prefix, fmt') = T.breakOn "%b" fmt
        in prefix <> showBool b <> T.drop 2 fmt'
    where showBool True  = "true"
          showBool False = "false"
