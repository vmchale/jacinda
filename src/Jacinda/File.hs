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
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)
import           Data.Bifunctor             (second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as ASCII
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCIIL
import           Data.Functor               (void, ($>))
import           Data.Tuple                 (swap)
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

parseLib :: FilePath -> StateT AlexUserState IO [D AlexPosn]
parseLib fp = do
    contents <- liftIO $ BSL.readFile fp
    st <- get
    case parseLibWithCtx contents st of
        Left err              -> liftIO (throwIO err)
        Right (st', ([], ds)) -> put st' $> (rewriteD <$> ds)
        Right (st', (is, ds)) -> do { put st' ; dss <- traverse parseLib is ; pure (concat dss ++ ds) }

parseE :: BSL.ByteString -> StateT AlexUserState IO (Program AlexPosn)
parseE bs = do
    st <- get
    case parseWithCtx bs st of
        Left err -> liftIO $ throwIO err
        Right (st', (is, Program ds e)) -> do
            put st'
            dss <- traverse parseLib is
            pure $ Program (concat dss ++ fmap rewriteD ds) (rewriteE e)

-- | Parse + rename (decls)
parseEWithMax :: BSL.ByteString -> IO (Program AlexPosn, Int)
parseEWithMax bsl = uncurry renamePGlobal . swap . second fst3 <$> runStateT (parseE bsl) alexInitUserState
    where fst3 (x, _, _) = x

-- | Parse + rename (globally)
parseWithMax' :: BSL.ByteString -> Either (ParseError AlexPosn) (Program AlexPosn, Int)
parseWithMax' = fmap (uncurry renamePGlobal . second (rewriteProgram . snd)) . parseWithMax

exprEval :: BSL.ByteString -> E (T K)
exprEval src =
    case parseWithMax' src of
        Left err -> throw err
        Right (ast, m) ->
            let (typed, i) = yeet $ runTypeM m (tyProgram ast)
            in closedProgram i (compileIn typed)

compileFS :: Maybe BS.ByteString -> RurePtr
compileFS (Just bs) = compileDefault bs
compileFS Nothing   = defaultRurePtr

runOnBytes :: FilePath
           -> BSL.ByteString -- ^ Program
           -> Maybe BS.ByteString -- ^ Field separator
           -> BSL.ByteString
           -> IO ()
runOnBytes fp src cliFS contents = do
    (ast, m) <- parseEWithMax src
    (typed, i) <- yeetIO $ runTypeM m (tyProgram ast)
    cont <- yeetIO $ runJac (ASCII.pack fp) (compileFS (cliFS <|> getFS ast)) i typed
    cont $ fmap BSL.toStrict (ASCIIL.lines contents)
    -- see: BSL.split, BSL.splitWith

runOnHandle :: BSL.ByteString -- ^ Program
            -> Maybe BS.ByteString -- ^ Field separator
            -> Handle
            -> IO ()
runOnHandle src cliFS = runOnBytes "(runOnBytes)" src cliFS <=< BSL.hGetContents

runOnFile :: BSL.ByteString
          -> Maybe BS.ByteString
          -> FilePath
          -> IO ()
runOnFile e fs fp = runOnBytes fp e fs =<< BSL.readFile fp

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
