module File ( tcIO
            , tySrc
            , runOnHandle
            , runOnFile
            , exprEval
            ) where

import           A
import           A.I
import           Control.Applicative        ((<|>))
import           Control.Exception          (Exception, throw, throwIO)
import           Control.Monad              ((<=<))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, get, put, runStateT)
import           Control.Recursion          (cata, embed)
import           Data.Bifunctor             (second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCIIL
import           Data.Foldable              (traverse_)
import           Data.Functor               (($>))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.IO               as TIO
import           Data.Tuple                 (swap)
import           Include
import           Jacinda.Backend.Const
import           Jacinda.Backend.P
import           Jacinda.Check.Field
import           Jacinda.Regex
import           L
import           Parser
import           Parser.Rw
import           R
import           Regex.Rure                 (RurePtr)
import           System.IO                  (Handle)
import           Ty

parseLib :: [FilePath] -> FilePath -> StateT AlexUserState IO [D AlexPosn]
parseLib incls fp = do
    contents <- liftIO $ TIO.readFile =<< resolveImport incls fp
    st <- get
    case parseLibWithCtx contents st of
        Left err              -> liftIO (throwIO err)
        Right (st', ([], ds)) -> put st' $> (rwD <$> ds)
        Right (st', (is, ds)) -> do { put st' ; dss <- traverse (parseLib incls) is ; pure (concat dss ++ fmap rwD ds) }

parseE :: [FilePath] -> T.Text -> StateT AlexUserState IO (Program AlexPosn)
parseE incls bs = do
    st <- get
    case parseWithCtx bs st of
        Left err -> liftIO $ throwIO err
        Right (st', (is, Program ds e)) -> do
            put st'
            dss <- traverse (parseLib incls) is
            pure $ Program (concat dss ++ fmap rwD ds) (rwE e)

-- | Parse + rename (decls)
parseEWithMax :: [FilePath] -> T.Text -> IO (Program AlexPosn, Int)
parseEWithMax incls bsl = uncurry rP . swap . second fst3 <$> runStateT (parseE incls bsl) alexInitUserState
    where fst3 (x, _, _) = x

parseWithMax' :: T.Text -> Either (ParseError AlexPosn) (Program AlexPosn, Int)
parseWithMax' = fmap (uncurry rP . second (rwP . snd)) . parseWithMax

type FileBS = BS.ByteString

tcompile=compileDefault.encodeUtf8

compileR :: FileBS
         -> E T
         -> E T
compileR fp = cata a where
    a (RegexLitF _ rrϵ) = RC (compileDefault rrϵ)
    a (NBF _ Fp)        = mkStr fp
    a x                 = embed x

exprEval :: T.Text -> E T
exprEval src =
    case parseWithMax' src of
        Left err -> throw err
        Right (ast, m) ->
            let (typed, i) = yeet $ runTyM m (tyP ast)
                (inlined, j) = ib i typed
            in eB j pure (compileR (error "nf not defined.") inlined)

compileFS :: Maybe T.Text -> RurePtr
compileFS = maybe defaultRurePtr tcompile

runOnBytes :: [FilePath]
           -> FilePath -- ^ Data file name, for @nf@
           -> T.Text -- ^ Program
           -> Maybe T.Text -- ^ Field separator
           -> Maybe T.Text -- ^ Record separator
           -> BSL.ByteString
           -> IO ()
runOnBytes incls fp src cliFS cliRS contents = do
    incls' <- defaultIncludes <*> pure incls
    (ast, m) <- parseEWithMax incls' src
    (typed, i) <- yIO $ runTyM m (tyP ast)
    let (eI, j) = ib i typed
    m'Throw $ cF eI
    let ~(afs, ars) = getS ast
    cont <- yIO $ runJac (compileFS (cliFS <|> afs)) (flushD typed) j (compileR (encodeUtf8 $ T.pack fp) eI)
    case cliRS <|> ars of
        Nothing -> cont $ fmap BSL.toStrict (ASCIIL.lines contents)
        Just rs -> cont $ lazySplit (tcompile rs) contents

runOnHandle :: [FilePath]
            -> T.Text -- ^ Program
            -> Maybe T.Text -- ^ Field separator
            -> Maybe T.Text -- ^ Record separator
            -> Handle
            -> IO ()
runOnHandle is src cliFS cliRS = runOnBytes is "(runOnBytes)" src cliFS cliRS <=< BSL.hGetContents

runOnFile :: [FilePath]
          -> T.Text
          -> Maybe T.Text
          -> Maybe T.Text
          -> FilePath
          -> IO ()
runOnFile is e fs rs fp = runOnBytes is fp e fs rs =<< BSL.readFile fp

tcIO :: [FilePath] -> T.Text -> IO ()
tcIO incls src = do
    incls' <- defaultIncludes <*> pure incls
    (ast, m) <- parseEWithMax incls' src
    (pT, i) <- yIO $ runTyM m (tyP ast)
    let (eI, _) = ib i pT
    m'Throw $ cF eI

tySrc :: T.Text -> T
tySrc src =
    case parseWithMax' src of
        Right (ast, m) -> yeet $ fst <$> runTyM m (tyOf (expr ast))
        Left err       -> throw err

m'Throw :: Exception e => Maybe e -> IO ()
m'Throw = traverse_ throwIO

yIO :: Exception e => Either e a -> IO a
yIO = either throwIO pure

yeet :: Exception e => Either e a -> a
yeet = either throw id
