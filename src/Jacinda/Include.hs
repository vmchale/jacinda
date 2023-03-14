module Jacinda.Include ( defaultIncludes
                       , resolveImport
                       ) where

import           Control.Exception  (Exception, throwIO)
import           Control.Monad      (filterM)
import           Data.List.Split    (splitWhen)
import           Data.Maybe         (listToMaybe)
import           Paths_jacinda      (getDataDir)
import           System.Directory   (doesFileExist, getCurrentDirectory)
import           System.Environment (lookupEnv)
import           System.FilePath    ((</>))

data ImportError = FileNotFound !FilePath ![FilePath] deriving (Show)

instance Exception ImportError where

defaultIncludes :: IO ([FilePath] -> [FilePath])
defaultIncludes = do
    path <- jacPath
    d <- getDataDir
    dot <- getCurrentDirectory
    pure $ (dot:) . (d:) . (++path)

jacPath :: IO [FilePath]
jacPath = maybe [] splitEnv <$> lookupEnv "JAC_PATH"

splitEnv :: String -> [FilePath]
splitEnv = splitWhen (== ':')

resolveImport :: [FilePath] -- ^ Places to look
              -> FilePath
              -> IO FilePath
resolveImport incl fp =
    maybe (throwIO $ FileNotFound fp incl) pure . listToMaybe
        =<< (filterM doesFileExist . fmap (</> fp) $ incl)
