module Jacinda.File ( tyCheck
                    , tcIO
                    , tySrc
                    , runOnHandle
                    , runOnFile
                    , exprEval
                    ) where

import           Control.Applicative        ((<|>))
import           Control.Exception          (Exception, throw, throwIO)
import           Control.Monad              ((<=<))
import           Data.Bifunctor             (second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCIIL
import           Data.Functor               (void)
import           Jacinda.AST
import           Jacinda.Backend.Normalize
import           Jacinda.Backend.TreeWalk
import           Jacinda.Lexer
import           Jacinda.Parser
import           Jacinda.Parser.Rewrite
import           Jacinda.Regex
import           Jacinda.Rename
import           Jacinda.Ty
import           Regex.Rure                 (RurePtr)
import           System.IO                  (Handle)

-- | Parse + rename (globally)
parseWithMax' :: BSL.ByteString -> Either (ParseError AlexPosn) (Program AlexPosn, Int)
parseWithMax' = fmap (uncurry renamePGlobal . second rewriteProgram) . parseWithMax

exprEval :: BSL.ByteString -> E (T K)
exprEval src =
    case parseWithMax' src of
        Left err -> throw err
        Right (ast, m) ->
            let (typed, i) = yeet $ runTypeM m (tyProgram ast)
            in closedProgram i typed

compileFS :: Maybe BS.ByteString -> RurePtr
compileFS (Just bs) = compileDefault bs
compileFS Nothing   = defaultRurePtr

runOnBytes :: BSL.ByteString -- ^ Program
           -> Maybe BS.ByteString -- ^ Field separator
           -> BSL.ByteString
           -> IO ()
runOnBytes src cliFS contents =
    case parseWithMax' src of
        Left err -> throwIO err
        Right (ast, m) -> do
            (typed, i) <- yeetIO $ runTypeM m (tyProgram ast)
            cont <- yeetIO $ runJac (compileFS (cliFS <|> getFS ast)) i typed
            cont $ concatMap BSL.toChunks (ASCIIL.lines contents) -- FIXME: "lines" discards empty... perhaps ok?

runOnHandle :: BSL.ByteString -- ^ Program
            -> Maybe BS.ByteString -- ^ Field separator
            -> Handle
            -> IO ()
runOnHandle src cliFS = runOnBytes src cliFS <=< BSL.hGetContents

runOnFile :: BSL.ByteString
          -> Maybe BS.ByteString
          -> FilePath
          -> IO ()
runOnFile e fs = runOnBytes e fs <=< BSL.readFile

tcIO :: BSL.ByteString -> IO ()
tcIO = yeetIO . tyCheck

-- | Typecheck an expression
tyCheck :: BSL.ByteString -> Either (Error AlexPosn) ()
tyCheck src =
    case parseWithMax' src of
        Right (ast, m) -> void $ runTypeM m (tyProgram ast)
        Left err       -> throw err

tySrc :: BSL.ByteString -> T K
tySrc src =
    case parseWithMax' src of
        Right (ast, m) -> yeet $ fst <$> runTypeM m (tyOf (expr ast))
        Left err       -> throw err

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

yeet :: Exception e => Either e a -> a
yeet = either throw id
