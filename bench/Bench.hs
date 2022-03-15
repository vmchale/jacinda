{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.DeepSeq    (NFData (..))
import           Criterion.Main
import           Jacinda.AST
import           Jacinda.File
import           System.IO.Silently (silence)

main :: IO ()
main =
    defaultMain [ bgroup "eval"
                      [ bench "exprEval" $ nf exprEval "[x+' '+y]|'' split '01-23-1987' /-/"
                      , bench "runOnFile" $ nfIO (silence $ runOnFile [] "(+)|0 {%/Bloom/}{1}" Nothing "bench/data/ulysses.txt")
                      ]
                ]

instance NFData (E a) where
    rnf (StrLit _ str)  = rnf str
    rnf (IntLit _ i)    = rnf i
    rnf (BoolLit _ b)   = rnf b
    rnf (FloatLit _ f)  = rnf f
    rnf (Arr _ es)      = rnf es
    rnf (Tup _ es)      = rnf es
    rnf (OptionVal _ e) = rnf e
