module Main (main) where

import           Criterion.Main
import qualified Data.ByteString.Lazy as BSL
import           Jacinda.File

main :: IO ()
main =
    defaultMain [ env (BSL.readFile "prelude/fn.jac") $ \contents ->
                  bgroup "pipeline"
                      [ bench "tcIO" $ nfIO (tcIO contents)
                      ]
                ]
