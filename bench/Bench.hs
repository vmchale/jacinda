{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           A
import           Control.DeepSeq    (NFData (..))
import           Criterion.Main
import qualified Data.Text.IO       as TIO
import           File
import           System.IO.Silently (silence)

main :: IO ()
main =
    defaultMain [ bgroup "eval"
                      [ bench "exprEval" $ nf exprEval "[x+' '+y]|'' split '01-23-1987' /-/"
                      , bench "exprEval" $ nf exprEval "reintercalate ' ' (split '01-23-1987' /-/)"
                      ]
                , bgroup "csv"
                      [ bench "dedup" $ nfIO (silence $ runOnFile [] "" "~.{ix>1}{`8}" [] CSV "bench/data/food-prices.csv")
                      , bench "succdiff" $ nfIO (silence $ runOnFile [] "" "(%)\\. {%/Apple/}{`3:}" [] CSV "bench/data/food-prices.csv")
                      ]
                , bgroup "stream"
                      [ bench "path" $ nfIO (silence $ runOnFile [] "" "{|[x+'\\n'+y]|>`$}" [] (AWK (Just ":") Nothing) "bench/data/PATH")
                      , bench "RS" $ nfIO (silence $ runOnFile [] "" "$0" [] (AWK Nothing (Just ":")) "bench/data/PATH")
                      , bench "runOnFile" $ nfIO (silence $ runOnFile [] "" "(+)|0 {%/Bloom/}{1}" [] (AWK Nothing Nothing) "bench/data/ulysses.txt")
                      , bench "runOnFile/wc.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/wc.jac" ; runOnFile [] "examples/wc.jac" contents [] (AWK Nothing Nothing) "bench/data/ulysses.txt" })
                      , bench "runOnFile/span2.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/span2.jac" ; runOnFile [] "examples/span2.jac" contents [] (AWK Nothing Nothing) "bench/data/span.txt" })
                      , bench "sedstream.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/sedstream.jac" ; runOnFile [] "examples/sedstream.jac" contents [] (AWK Nothing Nothing) "bench/data/lines.txt" })
                      , bench "gnused.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/gnused.jac" ; runOnFile [] "exampes/gnused.jac" contents [] (AWK Nothing Nothing) "bench/data/lines.txt" })
                      -- , bench "fungnused.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/fungnused.jac" ; runOnFile [] contents (AWK Nothing Nothing) "bench/data/lines.txt" })
                      , bench "hsLibversionMac.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/hsLibversionMac.jac"; runOnFile [] "examples/hsLibVersionMac.jac" contents [] (AWK Nothing Nothing) "bench/data/pandoc-mac" })
                      , bench "sedsmtp.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/sedsmtp.jac" ; runOnFile [] "examples/sedsmtp.jac" contents [] (AWK Nothing Nothing) "test/examples/data/2.txt" })
                      ]
                ]

instance NFData (E a) where
    rnf (Lit _ l) = rnf l
    rnf (Arr _ es) = rnf es; rnf (Tup _ es) = rnf es
    rnf (OptionVal _ e) = rnf e
