{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           A
import qualified Data.ByteString.Lazy as BSL
import           Control.DeepSeq (NFData (..))
import           Criterion.Main
import           Jacinda.Regex
import qualified Data.Text.IO    as TIO
import           File
import           System.IO       (IOMode (WriteMode), withFile)

hrun ifp e m fp = withFile "/dev/null" WriteMode $ \h -> runOnFile [] Nothing e [] m fp h
runs e m fp = nfIO $ hrun "(bench)" e m fp
fruns ifp m fp = nfIO $ do { contents <- TIO.readFile ifp; hrun ifp contents m fp }

main :: IO ()
main =
    defaultMain [ bgroup "eval"
                      [ bench "exprEval" $ nf exprEval "[x+' '+y]|'' split '01-23-1987' /-/"
                      , bench "exprEval" $ nf exprEval "reintercalate ' ' (split '01-23-1987' /-/)"
                      ]
                , bgroup "csv"
                      [ bench "dedup" $ runs "~.{ix>1}{`8}" CSV "bench/data/food-prices.csv"
                      , bench "succdiff" $ runs "(%)\\. {%/Apple/}{`3:}" CSV "bench/data/food-prices.csv"
                      ]
                , let rp=tcompile "\n\n"
                  in bgroup "rure"
                      [ bench "RS" $ nfIO (do {contents <- BSL.readFile "bench/data/ghc"; pure (lazySplit rp contents)})
                      , bench "header" $ nfIO (do {contents <- BSL.readFile "bench/data/ghc"; pure (lazySplitH rp contents)})
                      ]
                , bgroup "report"
                      [ bench "ghc-filt" $ fruns "test/examples/ghc-filt.jac" awk "test/data/ghc" ]
                , bgroup "stream"
                      [ bench "sprintf" $ runs "{%/infix(r|l)?\\s+\\d+/}{sprintf '- fixity: %s' `0}" awk "src/A.hs"
                      , bench "path" $ runs "{|[x+'\\n'+y]|>`$}" (AWK (Just ":") Nothing False) "bench/data/PATH"
                      , bench "RS" $ runs "$0" (AWK Nothing (Just ":") False) "bench/data/PATH"
                      , bench "runOnFile" $ runs "(+)|0 {%/Bloom/}{1}" awk "bench/data/ulysses.txt"
                      , bench "runOnFile/wc.jac" $ fruns "examples/wc.jac" awk "bench/data/ulysses.txt"
                      , bench "runOnFile/span2.jac" $ fruns "examples/span2.jac" awk "bench/data/span.txt"
                      , bench "sedstream.jac" $ fruns "examples/sedstream.jac" awk "bench/data/lines.txt"
                      , bench "gnused.jac" $ fruns "examples/gnused.jac" awk "bench/data/lines.txt"
                      -- , bench "fungnused.jac" $ fruns "examples/fungnused.jac" awk "bench/data/lines.txt" }
                      , bench "hsLibversionMac.jac" $ fruns "examples/hsLibversionMac.jac" awk "bench/data/pandoc-mac"
                      , bench "sedsmtp.jac" $ fruns "examples/sedsmtp.jac" awk "test/examples/data/2.txt"
                      ]
                ]

instance NFData (E a) where
    rnf (Lit _ l) = rnf l
    rnf (Arr _ es) = rnf es; rnf (Tup _ es) = rnf es
    rnf (OptionVal _ e) = rnf e
