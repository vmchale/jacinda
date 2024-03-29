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
                      [ bench "exprEval" $ nf exprEval "[x+' '+y]|'' split '01-23-1987' /-/" ]
                , bgroup "stream"
                      [ bench "path" $ nfIO (silence $ runOnFile [] "{|[x+'\\n'+y]|>`$}" (Just ":") Nothing "bench/data/PATH")
                      , bench "RS" $ nfIO (silence $ runOnFile [] "$0" Nothing (Just ":") "bench/data/PATH")
                      , bench "runOnFile" $ nfIO (silence $ runOnFile [] "(+)|0 {%/Bloom/}{1}" Nothing Nothing "bench/data/ulysses.txt")
                      , bench "runOnFile" $ nfIO (silence $ do { contents <- TIO.readFile "examples/wc.jac" ; runOnFile [] contents Nothing Nothing "bench/data/ulysses.txt" })
                      , bench "runOnFile" $ nfIO (silence $ do { contents <- TIO.readFile "examples/span2.jac" ; runOnFile [] contents Nothing Nothing "bench/data/span.txt" })
                      , bench "sedstream.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/sedstream.jac" ; runOnFile [] contents Nothing Nothing "bench/data/lines.txt" })
                      , bench "gnused.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/gnused.jac" ; runOnFile [] contents Nothing Nothing "bench/data/lines.txt" })
                      , bench "fungnused.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/fungnused.jac" ; runOnFile [] contents Nothing Nothing "bench/data/lines.txt" })
                      , bench "sedsmtp.jac" $ nfIO (silence $ do { contents <- TIO.readFile "examples/sedsmtp.jac" ; runOnFile [] contents Nothing Nothing "test/examples/data/2.txt" })
                      ]
                ]

instance NFData (E a) where
    rnf (Lit _ l) = rnf l
    rnf (Arr _ es) = rnf es; rnf (Tup _ es) = rnf es
    rnf (OptionVal _ e) = rnf e
