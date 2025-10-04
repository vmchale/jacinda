{-# LANGUAGE LambdaCase #-}

module Include ( defaultIncludes, resolveImport ) where

import           Control.Exception         (Exception, throwIO)
import           Control.Monad             (filterM, (<=<))
import           Data.Containers.ListUtils (nubOrd)
import           Data.List.Split           (splitWhen)
import           Paths_jacinda             (getDataDir)
import           System.Directory          (canonicalizePath, doesDirectoryExist, doesFileExist, getCurrentDirectory)
import           System.Environment        (lookupEnv)
import           System.FilePath           ((</>))

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
resolveImport incl fp = ($incl) $
    (\case [] -> throwIO $ FileNotFound fp incl; [src] -> pure (src</>fp); fs -> throwIO $ AmbiguousInclude fs)
    . nubOrd
        <=< traverse canonicalizePath
        <=< filterM (doesFileExist . (</> fp))
