module Jacinda.Env ( jacPath
                   ) where

import           Data.List.Split    (splitWhen)
import           System.Environment (lookupEnv)

-- | Parsed @JAC_PATH@
jacPath :: IO [FilePath]
jacPath = maybe [] splitEnv <$> lookupEnv "JAC_PATH"

splitEnv :: String -> [FilePath]
splitEnv = splitWhen (== ':')
