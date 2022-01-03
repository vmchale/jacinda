-- | Tree-walking interpreter, uses [io-streams](https://hackage.haskell.org/package/io-streams).
module Jacinda.Backend.IOStream ( runJac
                                -- * Field parsers
                                , readDigits
                                ) where

import           Control.Exception         (Exception)
import           Control.Monad             ((<=<))
import           Control.Recursion         (cata, embed)
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as ASCII
import           Data.IORef                (modifyIORef', newIORef, readIORef)
import qualified Data.Vector               as V
import           Jacinda.AST
import           Jacinda.Backend.Normalize
import           Jacinda.Regex
import           Jacinda.Ty.Const
import           Regex.Rure                (RureIterPtr, RurePtr)
import qualified System.IO.Streams         as Streams

data StreamError = FieldFile deriving (Show)

instance Exception StreamError where

readFloat :: BS.ByteString -> Double
readFloat = read . ASCII.unpack

readDigits :: BS.ByteString -> Integer
readDigits = ASCII.foldl' (\seed x -> 10 * seed + f x) 0
    where f '0' = 0
          f '1' = 1
          f '2' = 2
          f '3' = 3
          f '4' = 4
          f '5' = 5
          f '6' = 6
          f '7' = 7
          f '8' = 8
          f '9' = 9
          f c   = error (c:" is not a valid digit!")

noRes :: a
noRes = error "Internal error: called on an ill-typed expression?"

-- badSugar :: a
-- badSugar = error "Internal error: dfn syntactic sugar at a stage where it should not be."

asInt :: E a -> Integer
asInt (IntLit _ i) = i
asInt _            = noRes

asBool :: E a -> Bool
asBool (BoolLit _ b) = b
asBool _             = noRes

asStr :: E a -> BS.ByteString
asStr (StrLit _ str) = str
asStr _              = noRes

asFloat :: E a -> Double
asFloat (FloatLit _ f) = f
asFloat _              = noRes

-- eval
eEval :: (Int, BS.ByteString, V.Vector BS.ByteString) -- ^ Field context (for that line)
      -> E (T K)
      -> E (T K)
eEval (ix, line, ctx) = cata a where
    a b@BoolLitF{} = embed b
    a i@IntLitF{} = embed i
    a f@FloatLitF{} = embed f
    a str@StrLitF{} = embed str
    a rr@RegexLitF{} = embed rr
    a re@RegexCompiledF{} = embed re
    a op@BBuiltinF{} = embed op
    a op@UBuiltinF{} = embed op
    a op@TBuiltinF{} = embed op
    a e@(EAppF _ BBuiltin{} _) = embed e
    a AllFieldF{} = StrLit tyStr line
    a (FieldF _ i) = StrLit tyStr (ctx V.! (i-1)) -- cause vector indexing starts at 0
    a (IParseFieldF _ i) = IntLit tyI (readDigits $ ctx V.! (i-1))
    a (FParseFieldF _ i) = FloatLit tyF (readFloat $ ctx V.! (i-1))
    a (EAppF _ (EApp _ (BBuiltin _ Matches) e) e') =
        case (e, e') of
            (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (isMatch' re str)
            (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (isMatch' re str)
            _                                -> noRes
    a (EAppF _ (EApp _ (BBuiltin _ NotMatches) e) e') =
        case (e, e') of
            (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (not $ isMatch' re str)
            (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (not $ isMatch' re str)
            _                                -> noRes
    a (EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Times) e) e') =
        let eI = asFloat e
            eI' = asFloat e'
            in FloatLit tyF (eI * eI')
    a (EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Plus) e) e') =
        let eI = asInt e
            eI' = asInt e'
            in IntLit tyI (eI + eI')
    a (EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Gt) e) e') =
        let eI = asInt e
            eI' = asInt e'
            in BoolLit tyBool (eI > eI')
    a (EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Lt) e) e') =
        let eI = asInt e
            eI' = asInt e'
            in BoolLit tyBool (eI < eI')
    a (EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Eq) e) e') =
        let eI = asInt e
            eI' = asInt e'
            in BoolLit tyBool (eI == eI')
    a (EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Eq) e) e') =
        let eI = asStr e
            eI' = asStr e'
            in BoolLit tyBool (eI == eI')
    a (EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Neq) e) e') =
        let eI = asInt e
            eI' = asInt e'
            in BoolLit tyBool (eI == eI')
    a (EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Neq) e) e') =
        let eI = asStr e
            eI' = asStr e'
            in BoolLit tyBool (eI /= eI')
    a (EAppF _ (EApp _ (BBuiltin _ And) e) e') =
        let b = asBool e
            b' = asBool e'
            in BoolLit tyBool (b && b')
    a (EAppF _ (EApp _ (BBuiltin _ Or) e) e') =
        let b = asBool e
            b' = asBool e'
            in BoolLit tyBool (b || b')
    a (EAppF _ (UBuiltin _ Tally) e) =
        IntLit tyI (fromIntegral $ BS.length str)
        where str = asStr e
    a (TupF ty es) = Tup ty es

applyOp :: E (T K) -- ^ Operator
        -> E (T K)
        -> E (T K)
        -> E (T K)
applyOp op e e' = eNorm (EApp undefined (EApp undefined op e) e') -- FIXME: undefined is ??

atField :: Int
        -> BS.ByteString -- ^ Line
        -> BS.ByteString
atField i = (V.! (i-1)) . splitWhitespace

mkCtx :: Int -> BS.ByteString -> (Int, BS.ByteString, V.Vector BS.ByteString)
mkCtx ix line = (ix, line, splitWhitespace line)

applyUn :: E (T K)
        -> E (T K)
        -> E (T K)
applyUn unOp e =
    case eLoc unOp of
        TyArr _ _ res -> EApp res unOp e
        _             -> error "Internal error?"

imap :: (Int -> a -> b)
     -> Streams.InputStream a
     -> IO (Streams.InputStream b)
imap f inp = do
    ix <- newIORef 1
    Streams.mapM (\a -> do
        { ixϵ <- readIORef ix
        ; modifyIORef' ix (+1)
        ; pure (f ixϵ a)
        }) inp

ifilter :: (Int -> a -> Bool)
        -> Streams.InputStream a
        -> IO (Streams.InputStream a)
ifilter p inp = do
    ix <- newIORef 1
    Streams.filterM (\x -> do
        { ixϵ <- readIORef ix
        ; modifyIORef' ix (+1)
        ; pure (p ixϵ x)
        }) inp

-- eval stream expression
ir :: E (T K)
   -> Streams.InputStream BS.ByteString
   -> IO (Streams.InputStream (E (T K))) -- TODO: include chunks/context too?
ir AllColumn{}     = Streams.map mkStr
ir (Column _ i)    = Streams.map (mkStr . atField i)
ir (IParseCol _ i) = Streams.map (parseAsEInt . atField i)
ir (FParseCol _ i) = Streams.map (parseAsF . atField i)
ir (Guarded _ pe e) =
    let pe' = compileR pe
    -- FIXME: compile e too?
        in imap (\ix line -> eEval (mkCtx ix line) e) <=< ifilter (\ix line -> asBool (eEval (mkCtx ix line) pe'))
ir (EApp _ (EApp _ (BBuiltin _ Map) op) stream) = Streams.map (eNorm . applyUn (eNorm op)) <=< ir stream
ir (EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) op) streaml) streamr) = \lineStream -> do
    (inp0, inp1) <- dupStream lineStream
    irl <- ir streaml inp0
    irr <- ir streamr inp1
    Streams.zipWith (applyOp (eNorm op)) irl irr

mkStr :: BS.ByteString -- ^ Field
      -> E (T K)
mkStr = StrLit tyStr

parseAsEInt :: BS.ByteString -- ^ Field
            -> E (T K)
parseAsEInt = IntLit tyI . readDigits

parseAsF :: BS.ByteString -> E (T K)
parseAsF = FloatLit tyF . readFloat

-- | Output stream that prints each entry (expression)
printStream :: IO (Streams.OutputStream (E (T K)))
printStream = Streams.makeOutputStream (foldMap print)

dupStream :: Streams.InputStream a -> IO (Streams.InputStream a, Streams.InputStream a)
dupStream = Streams.unzip <=< Streams.map (\x -> (x, x)) -- aka join (,) 🤓

-- TODO: eNormal before runJac
runJac :: E (T K)
       -> Either StreamError (Streams.InputStream BS.ByteString -> IO ())
runJac AllField{}    = Left FieldFile
runJac Field{}       = Left FieldFile
runJac IParseField{} = Left FieldFile
runJac FParseField{} = Left FieldFile
runJac AllColumn{} = Right $ \inp -> do
    ps <- printStream
    Streams.connectTo ps =<< Streams.map mkStr inp
runJac (Column _ i) = Right $ \inp -> do
    ps <- printStream
    Streams.connectTo ps =<< Streams.map (mkStr . atField i) inp
-- TODO: this should extract any regex and compile them, use io/low-level API...
runJac e@Guarded{} = Right $ \inp -> do
    resStream <- ir e inp
    ps <- printStream
    Streams.connectTo ps resStream
runJac (EApp _ (EApp _ (EApp _ (TBuiltin _ Fold) op) seed) stream) = Right $ print <=< Streams.fold (applyOp op) seed <=< ir stream
runJac e@(EApp _ (EApp _ (BBuiltin _ Map) _) _) = Right $ \inp -> do
    resStream <- ir e inp
    ps <- printStream
    Streams.connectTo ps resStream
runJac e@(EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) _) _) _) = Right $ \inp -> do
    resStream <- ir e inp
    ps <- printStream
    Streams.connectTo ps resStream
runJac e@Let{} = runJac (eNorm e)
