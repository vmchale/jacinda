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
import           Data.Bifunctor             (second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCIIL
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
import           Text.CSV.Lazy.ByteString   (CSVField (..), parseCSV)
import           Ty

csvCtx :: BSL.ByteString -> [LineCtx]
csvCtx = go Nothing . parseCSV where
    go _ []                  = []
    go _ (Left err:_)        = error (show err)
    -- TODO: re-csv it?
    go (Just n) (Right r:rs) = let fs=mB<$>r in (fold fs, V.fromListN n fs, fromIntegral (line r)):go (Just n) rs
    go Nothing (Right r:rs)  = let fs=mB<$>r; n=length fs in (fold fs, V.fromListN n fs, fromIntegral (line r)):go (Just n) rs
    mB f@CSVField{} = BSL.toStrict (csvFieldContent f)
    mB f            = error (show f)
    line (f@CSVField{}:_) = csvRowNum f
    line (f:_)            = error (show f)
    line []               = error "empty record in csv"

parseLib :: [FilePath] -> FilePath -> StateT AlexUserState IO [D AlexPosn]
parseLib incls fp = do
    contents <- liftIO $ TIO.readFile =<< resolveImport incls fp
    st <- get
    case parseLibWithCtx contents st of
        Left err              -> liftIO (throwIO err)
        Right (st', ([], ds)) -> put st' $> (rwD <$> ds)
        Right (st', (is, ds)) -> do {put st'; dss <- traverse (parseLib incls) is; pure (concat dss ++ fmap rwD ds)}

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
compileR fp = r where
    r (RegexLit _ rrϵ)  = RC (compileDefault rrϵ)
    r (NB _ Fp)         = mkStr fp
    r e@Var{}           = e
    r e@UB{}            = e
    r e@NB{}            = e
    r e@Lit{}           = e
    r e@TB{}            = e
    r e@BB{}            = e
    r (Cond l p e0 e1)  = Cond l (r p) (r e0) (r e1)
    r (OptionVal l e)   = OptionVal l (r<$>e)
    r (EApp l e0 e1)    = EApp l (r e0) (r e1)
    r e@Column{}        = e
    r e@IParseCol{}     = e
    r e@IParseAllCol{}  = e
    r e@FParseAllCol{}  = e
    r e@ParseAllCol{}   = e
    r e@FParseCol{}     = e
    r e@ParseCol{}      = e
    r e@LastField{}     = e
    r e@Field{}         = e
    r e@FieldList{}     = e
    r e@AllField{}      = e
    r e@AllColumn{}     = e
    r (Guarded l p e)   = Guarded l (r p) (r e)
    r (Implicit l e)    = Implicit l (r e)
    r (Let l (n, eb) e) = Let l (n, r eb) (r e)
    r (Lam l n e)       = Lam l n (r e)
    r (Tup l es)        = Tup l (r<$>es)
    r (Arr l es)        = Arr l (r<$>es)
    r (Anchor l es)     = Anchor l (r<$>es)
    r (In l e0 e1 e)    = In l (r<$>e0) (r<$>e1) (r e)

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
    let (e', k) = runState (eta eI) j
        cont=run (flushD typed) k (compileR (encodeUtf8 $ T.pack fp) e')
    case (mode, getS ast) of
        (AWK cliFS cliRS, AWK afs ars) ->
            let r=compileFS (cliFS <|> afs)
                bs=case cliRS <|> ars of
                    Nothing -> fmap BSL.toStrict (ASCIIL.lines contents)
                    Just rs -> lazySplit (tcompile rs) contents
                ctxs=zipWith (\ ~(x,y) z -> (x,y,z)) [(b, splitBy r b) | b <- bs] [1..]
            in cont ctxs
        (CSV, _) -> let ctxs = csvCtx contents in cont ctxs
        (_, CSV) -> let ctxs = csvCtx contents in cont ctxs

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
