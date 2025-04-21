{-# LANGUAGE LambdaCase #-}

module Include ( defaultIncludes, resolveImport ) where

import           Control.Exception  (Exception, throwIO)
import           Control.Monad      (filterM)
import           Data.List.Split    (splitWhen)
import           Paths_jacinda      (getDataDir)
import           System.Directory   (doesDirectoryExist, doesFileExist, getCurrentDirectory)
import           System.Environment (lookupEnv)
import           System.FilePath    ((</>))

data ImportError = FileNotFound !FilePath ![FilePath] | AmbiguousInclude ![FilePath] deriving (Show)

instance Exception ImportError where

defaultIncludes :: IO ([FilePath] -> [FilePath])
defaultIncludes = do
    path <- jacPath
    d <- getDataDir
    dot <- getCurrentDirectory
    share <- doesDirectoryExist shareDir
    pure $ (if share then (shareDir:) else id).(dot:).(d:).(++path)
  where
    shareDir = "/usr/local/share/jac"

jacPath :: IO [FilePath]
jacPath = maybe [] splitEnv <$> lookupEnv "JAC_PATH"
  where
    splitEnv = splitWhen (== ':')

resolveImport :: [FilePath] -> FilePath -> IO FilePath
resolveImport incl fp =
    (\case [] -> throwIO $ FileNotFound fp incl; [fp] -> pure fp; fs -> throwIO $ AmbiguousInclude fs)
        =<< (filterM (doesFileExist . (</> fp)) $ incl)
