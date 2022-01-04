-- | Tree-walking interpreter, uses [io-streams](https://hackage.haskell.org/package/io-streams).
module Jacinda.Backend.IOStream ( runJac
                                ) where

-- TODO: normalize before mapping?

import           Control.Exception         (Exception)
import           Control.Monad             ((<=<))
import qualified Data.ByteString           as BS
import qualified Data.ByteString.Char8     as ASCII
import qualified Data.Vector               as V
import           Jacinda.AST
import           Jacinda.Backend.Normalize
import           Jacinda.Regex
import           Jacinda.Ty.Const
import           Regex.Rure                (RurePtr)
import qualified System.IO.Streams         as Streams
import           System.IO.Streams.Ext     as Streams

data StreamError = NakedField
                 | UnevalFun deriving (Show)

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

mkI :: Integer -> E (T K)
mkI = IntLit tyI -- TODO: do this for float, string, bool and put it in a common module?

-- TODO: do I want to interleave state w/ eNorm or w/e

-- eval
eEval :: (Int, BS.ByteString, V.Vector BS.ByteString) -- ^ Field context (for that line)
      -> E (T K)
      -> E (T K)
eEval allCtx@(ix, line, ctx) = go where
    go b@BoolLit{} = b
    go i@IntLit{} = i
    go f@FloatLit{} = f
    go str@StrLit{} = str
    go rr@RegexLit{} = rr
    go re@RegexCompiled{} = re
    go op@BBuiltin{} = op
    go op@UBuiltin{} = op
    go op@TBuiltin{} = op
    go (EApp ty op@BBuiltin{} e) = EApp ty op (eEval allCtx e)
    go Ix{} = mkI (fromIntegral ix)
    go AllField{} = StrLit tyStr line
    go (Field _ i) = StrLit tyStr (ctx V.! (i-1)) -- cause vector indexing starts at 0
    go (IParseField _ i) = mkI (readDigits $ ctx V.! (i-1))
    go (FParseField _ i) = FloatLit tyF (readFloat $ ctx V.! (i-1))
    go (EApp _ (EApp _ (BBuiltin _ Matches) e) e') =
        let eI = eEval allCtx e
            eI' = eEval allCtx e'
        in case (eI, eI') of
            (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (isMatch' re str)
            (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (isMatch' re str)
            _                                -> noRes
    go (EApp _ (EApp _ (BBuiltin _ NotMatches) e) e') =
        let eI = eEval allCtx e
            eI' = eEval allCtx e'
        in case (eI, eI') of
            (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (not $ isMatch' re str)
            (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (not $ isMatch' re str)
            _                                -> noRes
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Times) e) e') =
        let eI = asFloat (eEval allCtx e)
            eI' = asFloat (eEval allCtx e')
            in FloatLit tyF (eI * eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Plus) e) e') =
        let eI = asInt (eEval allCtx e)
            eI' = asInt (eEval allCtx e')
            in mkI (eI + eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Plus) e) e') =
        let eI = asStr (eEval allCtx e)
            eI' = asStr (eEval allCtx e')
            in StrLit tyI (eI <> eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Gt) e) e') =
        let eI = asInt (eEval allCtx e)
            eI' = asInt (eEval allCtx e')
            in BoolLit tyBool (eI > eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Lt) e) e') =
        let eI = asInt (eEval allCtx e)
            eI' = asInt (eEval allCtx e')
            in BoolLit tyBool (eI < eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Eq) e) e') =
        let eI = asInt (eEval allCtx e)
            eI' = asInt (eEval allCtx e')
            in BoolLit tyBool (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Eq) e) e') =
        let eI = asStr (eEval allCtx e)
            eI' = asStr (eEval allCtx e')
            in BoolLit tyBool (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Neq) e) e') =
        let eI = asInt (eEval allCtx e)
            eI' = asInt (eEval allCtx e')
            in BoolLit tyBool (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Neq) e) e') =
        let eI = asStr (eEval allCtx e)
            eI' = asStr (eEval allCtx e')
            in BoolLit tyBool (eI /= eI')
    go (EApp _ (EApp _ (BBuiltin _ And) e) e') =
        let b = asBool (eEval allCtx e)
            b' = asBool (eEval allCtx e')
            in BoolLit tyBool (b && b')
    go (EApp _ (EApp _ (BBuiltin _ Or) e) e') =
        let b = asBool e
            b' = asBool e'
            in BoolLit tyBool (b || b')
    go (EApp _ (UBuiltin _ Tally) e) =
        mkI (fromIntegral $ BS.length str)
        where str = asStr (eEval allCtx e)
    go (Tup ty es) = Tup ty (eEval allCtx <$> es)

applyOp :: Int
        -> E (T K) -- ^ Operator
        -> E (T K)
        -> E (T K)
        -> E (T K)
applyOp i op e e' = eClosed i (EApp undefined (EApp undefined op e) e') -- FIXME: undefined is ??

atField :: RurePtr
        -> Int
        -> BS.ByteString -- ^ Line
        -> BS.ByteString
atField re i = (V.! (i-1)) . splitBy re

mkCtx :: RurePtr -> Int -> BS.ByteString -> (Int, BS.ByteString, V.Vector BS.ByteString)
mkCtx re ix line = (ix, line, splitBy re line)

applyUn :: E (T K)
        -> E (T K)
        -> E (T K)
applyUn unOp e =
    case eLoc unOp of
        TyArr _ _ res -> EApp res unOp e
        _             -> error "Internal error?"

-- | eval stream expression using line as context
ir :: RurePtr
   -> Int
   -> E (T K)
   -> Streams.InputStream BS.ByteString
   -> IO (Streams.InputStream (E (T K))) -- TODO: include chunks/context too?
ir _ _ AllColumn{}     = Streams.map mkStr
ir re _ (Column _ i)    = Streams.map (mkStr . atField re i)
ir re _ (IParseCol _ i) = Streams.map (parseAsEInt . atField re i)
ir re _ (FParseCol _ i) = Streams.map (parseAsF . atField re i)
ir re _ (Guarded _ pe e) =
    let pe' = compileR pe
    -- FIXME: compile e too?
    -- TODO: normalize before stream
        in imap (\ix line -> eEval (mkCtx re ix line) e) <=< ifilter (\ix line -> asBool (eEval (mkCtx re ix line) pe'))
ir re i (EApp _ (EApp _ (BBuiltin _ Map) op) stream) = let op' = eClosed i op in Streams.map (eClosed i . applyUn op') <=< ir re i stream
ir re i (EApp _ (EApp _ (BBuiltin _ Prior) op) stream) = let op' = eClosed i op in Streams.prior (applyOp i op') <=< ir re i stream
ir re i (EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) op) streaml) streamr) = \lineStream -> do
    (inp0, inp1) <- dupStream lineStream
    irl <- ir re i streaml inp0
    irr <- ir re i streamr inp1
    let op' = eClosed i op
    Streams.zipWith (applyOp i op') irl irr
ir re i (EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) op) seed) xs) = \inp -> do
    let op' = eClosed i op
    Streams.scan (applyOp i op') (eClosed i seed) =<< ir re i xs inp

mkStr :: BS.ByteString -- ^ Field
      -> E (T K)
mkStr = StrLit tyStr

parseAsEInt :: BS.ByteString -- ^ Field
            -> E (T K)
parseAsEInt = mkI . readDigits

parseAsF :: BS.ByteString -> E (T K)
parseAsF = FloatLit tyF . readFloat

-- | Output stream that prints each entry (expression)
printStream :: IO (Streams.OutputStream (E (T K)))
printStream = Streams.makeOutputStream (foldMap print)

foldWithCtx :: RurePtr -> Int
            -> E (T K)
            -> E (T K)
            -> E (T K)
            -> Streams.InputStream BS.ByteString
            -> IO (E (T K))
foldWithCtx re i op seed streamExpr = Streams.fold (applyOp i op) (eClosed i seed) <=< ir re i streamExpr

-- TODO: eClosed before runJac or w/e
--
-- TODO: passing in 'i' separately to each eClosed is sketch but... hopefully
-- won't blow up in our faces idk
runJac :: RurePtr
       -> Int
       -> E (T K)
       -> Either StreamError (Streams.InputStream BS.ByteString -> IO ())
runJac _ _ AllField{}    = Left NakedField
runJac _ _ Field{}       = Left NakedField
runJac _ _ IParseField{} = Left NakedField
runJac _ _ FParseField{} = Left NakedField
runJac _ _ AllColumn{} = Right $ \inp -> do
    ps <- printStream
    Streams.connectTo ps =<< Streams.map mkStr inp
runJac re _ (Column _ i) = Right $ \inp -> do
    ps <- printStream
    Streams.connectTo ps =<< Streams.map (mkStr . atField re i) inp
runJac re _ (IParseCol _ i) = Right $ \inp -> do
    ps <- printStream
    Streams.connectTo ps =<< Streams.map (parseAsEInt . atField re i) inp
runJac re _ (FParseCol _ i) = Right $ \inp -> do
    ps <- printStream
    Streams.connectTo ps =<< Streams.map (parseAsF . atField re i) inp
-- TODO: this should extract any regex and compile them, use io/low-level API...
runJac re i e@Guarded{} = Right $ \inp -> do
    resStream <- ir re i e inp
    ps <- printStream
    Streams.connectTo ps resStream
runJac re i (EApp _ (EApp _ (EApp _ (TBuiltin _ Fold) op) seed) stream) = Right $ print <=< foldWithCtx re i op seed stream
runJac re i e@(EApp _ (EApp _ (BBuiltin _ Map) _) _) = Right $ \inp -> do
    resStream <- ir re i e inp
    ps <- printStream
    Streams.connectTo ps resStream
runJac re i e@(EApp _ (EApp _ (BBuiltin _ Prior) _) _) = Right $ \inp -> do
    resStream <- ir re i e inp
    ps <- printStream
    Streams.connectTo ps resStream
runJac re i e@(EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) _) _) _) = Right $ \inp -> do
    resStream <- ir re i e inp
    ps <- printStream
    Streams.connectTo ps resStream
runJac re i e@(EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) _) _) _) = Right $ \inp -> do
    resStream <- ir re i e inp
    ps <- printStream
    Streams.connectTo ps resStream
runJac re i e@Let{} = runJac re i (eClosed i e)
runJac _ _ Var{} = error "Internal error?"
runJac _ _ e@IntLit{} = Right $ const (print e)
runJac _ _ e@BoolLit{} = Right $ const (print e)
runJac _ _ e@StrLit{} = Right $ const (print e)
runJac _ _ e@FloatLit{} = Right $ const (print e)
runJac _ _ e@RegexLit{} = Right $ const (print e)
runJac _ _ Lam{} = Left UnevalFun
runJac _ _ Dfn{} = desugar
runJac _ _ ResVar{} = desugar
runJac _ _ BBuiltin{} = Left UnevalFun
runJac _ _ UBuiltin{} = Left UnevalFun
runJac _ _ TBuiltin{} = Left UnevalFun
runJac re i e@(EApp _ Lam{} _) = runJac re i (eClosed i e)
