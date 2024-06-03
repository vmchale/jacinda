module File ( tcIO
            , tySrc
            , runStdin
            , runOnFile
            , exprEval
            ) where

import           A
import           A.E
import           A.I
import           Control.Applicative        ((<|>))
import           Control.Exception          (Exception, throw, throwIO)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.State.Strict (StateT, get, put, runState, runStateT)
import           Control.Recursion          (cata, embed)
import           Data.Bifunctor             (second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCIIL
import           Data.Csv.Streaming         (HasHeader (..), Records (..), decode)
import           Data.Foldable              (fold, traverse_)
import           Data.Functor               (($>))
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import qualified Data.Text.IO               as TIO
import           Data.Tuple                 (swap)
import qualified Data.Vector                as V
import           Include
import           Jacinda.Backend.Const
import           Jacinda.Backend.T
import           Jacinda.Check.Field
import           Jacinda.Regex
import           L
import           Parser
import           Parser.Rw
import           R
import           Regex.Rure                 (RurePtr)
import           System.IO                  (stdin)
import           Ty

csvCtx :: BSL.ByteString -> [(BS.ByteString, V.Vector BS.ByteString)]
csvCtx bs = let rs=decode NoHeader bs in go rs where
    go (Nil Nothing _)      = []
    go (Nil (Just s) _)     = error s
    go (Cons (Right ds) rs) = (fold ds, ds):go rs

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
            in eB j (compileR (error "nf not defined.") inlined)

compileFS :: Maybe T.Text -> RurePtr
compileFS = maybe defaultRurePtr tcompile

runOnBytes :: [FilePath]
           -> FilePath -- ^ Data file name, for @nf@
           -> T.Text -- ^ Program
           -> Mode
           -> BSL.ByteString
           -> IO ()
runOnBytes incls fp src mode contents = do
    incls' <- defaultIncludes <*> pure incls
    (ast, m) <- parseEWithMax incls' src
    (typed, i) <- yIO $ runTyM m (tyP ast)
    let (eI, j) = ib i typed
    m'Throw $ cF eI
    let ~(AWK afs ars) = getS ast
        (e', k) = runState (eta eI) j
        cont=run (flushD typed) k (compileR (encodeUtf8 $ T.pack fp) e')
    case mode of
        AWK cliFS cliRS ->
            let r=compileFS (cliFS <|> afs)
                bs=case cliRS <|> ars of
                    Nothing -> fmap BSL.toStrict (ASCIIL.lines contents)
                    Just rs -> lazySplit (tcompile rs) contents
                ctxs=zipWith (\ ~(x,y) z -> (x,y,z)) [(b, splitBy r b) | b <- bs] [1..]
            in cont ctxs
        CSV -> let ctxs = zipWith (\ ~(x,y) z -> (x,y,z)) (csvCtx contents) [1..] in cont ctxs

runStdin :: [FilePath]
         -> T.Text -- ^ Program
         -> Mode
         -> IO ()
runStdin is src m = runOnBytes is "(stdin)" src m =<< BSL.hGetContents stdin

runOnFile :: [FilePath]
          -> T.Text
          -> Mode
          -> FilePath
          -> IO ()
runOnFile is e m fp = runOnBytes is fp e m =<< BSL.readFile fp

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
