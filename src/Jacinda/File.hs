module Jacinda.File ( tyCheck
                    , tcIO
                    , tySrc
                    , runOnHandle
                    , runOnFile
                    ) where

import           Control.Exception        (Exception, throw, throwIO)
import           Data.Bifunctor           (second)
import qualified Data.ByteString.Lazy     as BSL
import           Data.Functor             (void)
import           Jacinda.AST
import           Jacinda.Backend.IOStream
import           Jacinda.Lexer
import           Jacinda.Parser
import           Jacinda.Parser.Rewrite
import           Jacinda.Rename
import           Jacinda.Ty
import           System.IO                (Handle, IOMode (ReadMode), withFile)
import qualified System.IO.Streams        as Streams

parseWithMax' :: BSL.ByteString -> Either (ParseError AlexPosn) (E AlexPosn, Int)
parseWithMax' = fmap (uncurry renameEGlobal . second rewriteE) . parseWithMax

runOnHandle :: BSL.ByteString -- ^ Expression
            -> Handle
            -> IO ()
runOnHandle src h =
    case parseWithMax' src of
        Left err -> throwIO err
        Right (ast, m) -> do
            typed <- yeetIO $ runTypeM m (tyE ast)
            cont <- yeetIO $ runJac typed
            cont =<< (Streams.handleToInputStream h >>= Streams.lines)

runOnFile :: BSL.ByteString
          -> FilePath
          -> IO ()
runOnFile e fp = withFile fp ReadMode $ runOnHandle e

tcIO :: BSL.ByteString -> IO ()
tcIO = yeetIO . tyCheck

-- | Typecheck an expression
tyCheck :: BSL.ByteString -> Either (Error AlexPosn) ()
tyCheck src =
    case parseWithMax' src of
        Right (ast, m) -> void $ runTypeM m (tyOf ast)
        Left err       -> throw err

tySrc :: BSL.ByteString -> T K
tySrc src =
    case parseWithMax' src of
        Right (ast, m) -> yeet $ runTypeM m (tyOf ast)
        Left err       -> throw err

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

yeet :: Exception e => Either e a -> a
yeet = either throw id
