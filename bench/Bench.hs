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
                      , bench "runOnFile" $ nfIO (silence $ runOnFile [] "(+)|0 {%/Bloom/}{1}" Nothing Nothing "bench/data/ulysses.txt")
                      , bench "runOnFile" $ nfIO (silence $ do { contents <- TIO.readFile "examples/wc.jac" ; runOnFile [] contents Nothing Nothing "bench/data/ulysses.txt" })
                      , bench "runOnFile" $ nfIO (silence $ do { contents <- TIO.readFile "examples/span2.jac" ; runOnFile [] contents Nothing Nothing "bench/data/span.txt" })
                      ]
                ]

instance NFData (E a) where
    rnf (Lit _ l) = rnf l
    rnf (Arr _ es) = rnf es; rnf (Tup _ es) = rnf es
    rnf (OptionVal _ e) = rnf e
