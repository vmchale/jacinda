module Jacinda.File ( tcIO
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
import           Control.Recursion          (cata, embed)
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
import           Jacinda.Include
import           Jacinda.Lexer
import           Jacinda.Parser
import           Jacinda.Parser.Rewrite
import           Jacinda.Regex
import           Jacinda.Rename
import           Jacinda.Ty
import           Regex.Rure                 (RurePtr)
import           System.IO                  (Handle)

parseLib :: [FilePath] -> FilePath -> StateT AlexUserState IO [D AlexPosn]
parseLib incls fp = do
    contents <- liftIO $ BSL.readFile =<< resolveImport incls fp
    st <- get
    case parseLibWithCtx contents st of
        Left err              -> liftIO (throwIO err)
        Right (st', ([], ds)) -> put st' $> (rewriteD <$> ds)
        Right (st', (is, ds)) -> do { put st' ; dss <- traverse (parseLib incls) is ; pure (concat dss ++ fmap rewriteD ds) }

parseE :: [FilePath] -> BSL.ByteString -> StateT AlexUserState IO (Program AlexPosn)
parseE incls bs = do
    st <- get
    case parseWithCtx bs st of
        Left err -> liftIO $ throwIO err
        Right (st', (is, Program ds e)) -> do
            put st'
            dss <- traverse (parseLib incls) is
            pure $ Program (concat dss ++ fmap rewriteD ds) (rewriteE e)

-- | Parse + rename (decls)
parseEWithMax :: [FilePath] -> BSL.ByteString -> IO (Program AlexPosn, Int)
parseEWithMax incls bsl = uncurry renamePGlobal . swap . second fst3 <$> runStateT (parseE incls bsl) alexInitUserState
    where fst3 (x, _, _) = x

-- | Parse + rename (globally)
parseWithMax' :: BSL.ByteString -> Either (ParseError AlexPosn) (Program AlexPosn, Int)
parseWithMax' = fmap (uncurry renamePGlobal . second (rewriteProgram . snd)) . parseWithMax

type FileBS = BS.ByteString

-- fill in regex with compiled.
compileR :: FileBS
         -> E (T K)
         -> E (T K)
compileR fp = cata a where
    a (RegexLitF _ rrϵ) = RegexCompiled (compileDefault rrϵ)
    a (NBuiltinF _ Fp)  = mkStr fp
    a x                 = embed x

compileIn :: FileBS -> Program (T K) -> Program (T K)
compileIn fp (Program ds e) = Program (compileD fp <$> ds) (compileR fp e)

compileD :: FileBS -> D (T K) -> D (T K)
compileD fp (FunDecl n l e) = FunDecl n l (compileR fp e)
compileD _ d                = d

exprEval :: BSL.ByteString -> E (T K)
exprEval src =
    case parseWithMax' src of
        Left err -> throw err
        Right (ast, m) ->
            let (typed, i) = yeet $ runTypeM m (tyProgram ast)
            in closedProgram i (compileIn undefined typed)

compileFS :: Maybe BS.ByteString -> RurePtr
compileFS (Just bs) = compileDefault bs
compileFS Nothing   = defaultRurePtr

runOnBytes :: [FilePath]
           -> FilePath -- ^ Data file name, for @nf@
           -> BSL.ByteString -- ^ Program
           -> Maybe BS.ByteString -- ^ Field separator
           -> BSL.ByteString
           -> IO ()
runOnBytes incls fp src cliFS contents = do
    incls' <- defaultIncludes <*> pure incls
    (ast, m) <- parseEWithMax incls' src
    (typed, i) <- yeetIO $ runTypeM m (tyProgram ast)
    cont <- yeetIO $ runJac (compileFS (cliFS <|> getFS ast)) i (compileIn (ASCII.pack fp) typed)
    cont $ fmap BSL.toStrict (ASCIIL.lines contents)
    -- TODO: BSL.split, BSL.splitWith for arbitrary record separators

runOnHandle :: [FilePath]
            -> BSL.ByteString -- ^ Program
            -> Maybe BS.ByteString -- ^ Field separator
            -> Handle
            -> IO ()
runOnHandle is src cliFS = runOnBytes is "(runOnBytes)" src cliFS <=< BSL.hGetContents

runOnFile :: [FilePath]
          -> BSL.ByteString
          -> Maybe BS.ByteString
          -> FilePath
          -> IO ()
runOnFile is e fs fp = runOnBytes is fp e fs =<< BSL.readFile fp

tcIO :: [FilePath] -> BSL.ByteString -> IO ()
tcIO incls src = do
    incls' <- defaultIncludes <*> pure incls
    (ast, m) <- parseEWithMax incls' src
    yeetIO $ void $ runTypeM m (tyProgram ast)

tySrc :: BSL.ByteString -> T K
tySrc src =
    case parseWithMax' src of
        Right (ast, m) -> yeet $ fst <$> runTypeM m (tyOf (expr ast))
        Left err       -> throw err

yeetIO :: Exception e => Either e a -> IO a
yeetIO = either throwIO pure

yeet :: Exception e => Either e a -> a
yeet = either throw id
