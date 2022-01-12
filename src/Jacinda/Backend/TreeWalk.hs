-- | Tree-walking interpreter
module Jacinda.Backend.TreeWalk ( runJac
                                ) where

-- TODO: normalize before mapping?

import           Control.Exception         (Exception, throw)
import qualified Data.ByteString           as BS
import           Data.Foldable             (foldl', traverse_)
import           Data.List                 (scanl')
import           Data.List.Ext
import           Data.Semigroup            ((<>))
import qualified Data.Vector               as V
import           Jacinda.AST
import           Jacinda.Backend.Normalize
import           Jacinda.Backend.Printf
import           Jacinda.Regex
import           Jacinda.Ty.Const
import           Regex.Rure                (RurePtr)

data StreamError = NakedField
                 | UnevalFun
                 | TupOfStreams -- ^ Reject a tuple of streams
                 | BadCtx
                 | IndexOutOfBounds Int
                 deriving (Show)

instance Exception StreamError where

(!) :: V.Vector a -> Int -> a
v ! ix = case v V.!? ix of
    Just x  -> x
    Nothing -> throw $ IndexOutOfBounds ix

noRes :: a
noRes = error "Internal error: did not normalize to appropriate type."

badSugar :: a
badSugar = error "Internal error: dfn syntactic sugar at a stage where it should not be."

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

asRegex :: E a -> RurePtr
asRegex (RegexCompiled re) = re
asRegex _                  = noRes

asArr :: E a -> V.Vector (E a)
asArr (Arr _ es) = es
asArr _          = noRes

-- TODO: do I want to interleave state w/ eNorm or w/e

-- eval
eEval :: (Int, BS.ByteString, V.Vector BS.ByteString) -- ^ Field context (for that line)
      -> E (T K)
      -> E (T K)
eEval (ix, line, ctx) = go where
    go b@BoolLit{} = b
    go i@IntLit{} = i
    go f@FloatLit{} = f
    go str@StrLit{} = str
    go rr@RegexLit{} = rr
    go reϵ@RegexCompiled{} = reϵ
    go op@BBuiltin{} = op
    go op@UBuiltin{} = op
    go op@TBuiltin{} = op
    go (EApp ty op@BBuiltin{} e) = EApp ty op (go e)
    go (NBuiltin _ Ix) = mkI (fromIntegral ix)
    go AllField{} = StrLit tyStr line
    go (Field _ i) = StrLit tyStr (ctx ! (i-1)) -- cause vector indexing starts at 0
    go (EApp _ (UBuiltin _ IParse) e) =
        let eI = asStr (go e)
            in parseAsEInt eI
    go (EApp _ (UBuiltin _ FParse) e) =
        let eI = asStr (go e)
            in parseAsF eI
    go (EApp _ (EApp _ (BBuiltin _ Matches) e) e') =
        let eI = go e
            eI' = go e'
        in case (eI, eI') of
            (RegexCompiled reϵ, StrLit _ strϵ) -> BoolLit tyBool (isMatch' reϵ strϵ)
            (StrLit _ strϵ, RegexCompiled reϵ) -> BoolLit tyBool (isMatch' reϵ strϵ)
            _                                  -> noRes
    go (EApp _ (EApp _ (BBuiltin _ NotMatches) e) e') =
        let eI = go e
            eI' = go e'
        in case (eI, eI') of
            (RegexCompiled reϵ, StrLit _ strϵ) -> BoolLit tyBool (not $ isMatch' reϵ strϵ)
            (StrLit _ strϵ, RegexCompiled reϵ) -> BoolLit tyBool (not $ isMatch' reϵ strϵ)
            _                                  -> noRes
    go (EApp _ (EApp _ (BBuiltin _ Match) e) e') =
        let eI = asRegex (go e)
            eI' = asStr (go e')
        in asTup (find' eI eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Plus) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in mkI (eI + eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Minus) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in mkI (eI - eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Times) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in mkI (eI * eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Plus) e) e') =
        let eI = asStr (go e)
            eI' = asStr (go e')
            in mkStr (eI <> eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Eq) e) e') =
        let eI = asStr (go e)
            eI' = asStr (go e')
            in BoolLit tyBool (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Gt) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyBool (eI > eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Lt) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyBool (eI < eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Eq) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyBool (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Neq) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyBool (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Neq) e) e') =
        let eI = asStr (go e)
            eI' = asStr (go e')
            in BoolLit tyBool (eI /= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Leq) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyBool (eI <= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Geq) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyBool (eI <= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Eq) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in BoolLit tyBool (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Neq) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in BoolLit tyBool (eI /= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Plus) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in mkF (eI + eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Minus) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in mkF (eI - eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Times) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in FloatLit tyF (eI * eI')
    go (EApp _ (EApp _ (BBuiltin _ Div) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in FloatLit tyF (eI / eI')
    go (EApp _ (EApp _ (BBuiltin _ And) e) e') =
        let b = asBool (go e)
            b' = asBool (go e')
            in BoolLit tyBool (b && b')
    go (EApp _ (EApp _ (BBuiltin _ Or) e) e') =
        let b = asBool e
            b' = asBool e'
            in BoolLit tyBool (b || b')
    go (EApp _ (UBuiltin _ Tally) e) =
        mkI (fromIntegral $ BS.length str)
        where str = asStr (go e)
    go (EApp _ (UBuiltin _ Floor) e) =
        let f = asFloat e
        in mkI (floor f)
    go (EApp _ (UBuiltin _ Ceiling) e) =
        let f = asFloat e
        in mkI (ceiling f)
    go (Tup ty es) = Tup ty (go <$> es)
    go (EApp _ (EApp _ (BBuiltin _ Split) e) e') =
        let str = asStr (go e)
            re = asRegex (go e')
            bss = splitBy re str
            in Arr undefined (StrLit undefined <$> bss)
    go (EApp _ (EApp _ (BBuiltin _ Splitc) e) e') =
        let str = asStr (go e)
            c = the (asStr (go e'))
            bss = BS.split c str
            in Arr undefined (StrLit undefined <$> V.fromList bss)
    go (EApp _ (EApp _ (EApp _ (TBuiltin _ Substr) e0) e1) e2) =
        let eI0 = asStr (go e0)
            eI1 = asInt (go e1)
            eI2 = asInt (go e2)
        in mkStr (substr eI0 (fromIntegral eI1) (fromIntegral eI2))
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Max) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in mkF (max eI eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Min) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in mkF (min eI eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Max) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in mkI (max eI eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Min) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in mkI (min eI eI')
    go (EApp _ (UBuiltin _ Not) e) =
        let eI = asBool (go e)
        in BoolLit tyBool (not eI)
    go (EApp _ (UBuiltin _ (At i)) e) =
        let eI = go e
            in case eI of
                (Arr _ es) -> go (es V.! (i-1))
                _          -> noRes
    go (EApp _ (UBuiltin _ (Select i)) e) =
        let eI = go e
            in case eI of
                (Tup _ es) -> go (es !! (i-1))
                _          -> noRes
    go (EApp _ (EApp _ (BBuiltin _ Sprintf) e) e') =
        let eI = asStr (go e)
            eI' = go e'
        in mkStr (sprintf eI eI')
    go (OptionVal ty e) =
        OptionVal ty (go <$> e)
    go (EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyVec) _))) Map) x) y) =
        let x' = go x
            y' = asArr (go y)
        in Arr undefined (applyUn' x' <$> y')
        where applyUn' :: E (T K) -> E (T K) -> E (T K)
              applyUn' e e' = go (EApp undefined e e')
    go (EApp _ (EApp _ (EApp _ (TBuiltin (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyVec) _) _))) Fold) f) seed) xs) =
        let f' = go f
            seed' = go seed
            xs' = asArr (go xs)
        in foldE f' seed' xs'
        where foldE op = V.foldl' (applyOp' op)
              applyOp' op e e' = go (EApp undefined (EApp undefined op e) e')
    go (Arr ty es) = Arr ty (go <$> es)

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
atField re i = (! (i-1)) . splitBy re

mkCtx :: RurePtr -> Int -> BS.ByteString -> (Int, BS.ByteString, V.Vector BS.ByteString)
mkCtx re ix line = (ix, line, splitBy re line)

applyUn :: Int
        -> E (T K)
        -> E (T K)
        -> E (T K)
applyUn i unOp e =
    case eLoc unOp of
        TyArr _ _ res -> eClosed i (EApp res unOp e)
        _             -> error "Internal error?"

-- | Turn an expression representing a stream into a stream of expressions (using line as context)
ir :: RurePtr
   -> Int
   -> E (T K)
   -> [BS.ByteString]
   -> [E (T K)] -- TODO: include chunks/context too?
ir _ _ AllColumn{} = fmap mkStr
ir re _ (Column _ i) = fmap (mkStr . atField re i)
ir re _ (IParseCol _ i) = fmap (parseAsEInt . atField re i)
ir re _ (FParseCol _ i) = fmap (parseAsF . atField re i)
ir re _ (Implicit _ e) =
    let e' = compileR e
        in imap (\ix line -> eEval (mkCtx re ix line) e')
ir re _ (Guarded _ pe e) =
    let pe' = compileR pe
        e' = compileR e
    -- FIXME: compile e too?
    -- TODO: normalize before stream
        in imap (\ix line -> eEval (mkCtx re ix line) e') . ifilter (\ix line -> asBool (eEval (mkCtx re ix line) pe'))
ir re i (EApp _ (EApp _ (BBuiltin _ Map) op) stream) = let op' = compileR op in fmap (applyUn i op') . ir re i stream
ir re i (EApp _ (EApp _ (BBuiltin _ Filter) op) stream) =
    let op' = compileR op
        in filter (asBool . applyUn i op') . ir re i stream
ir re i (EApp _ (EApp _ (BBuiltin _ Prior) op) stream) = prior (applyOp i op) . ir re i stream
ir re i (EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) op) streaml) streamr) = \lineStream ->
    let
        irl = ir re i streaml lineStream
        irr = ir re i streamr lineStream
    in zipWith (applyOp i op) irl irr
ir re i (EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) op) seed) xs) =
    scanl' (applyOp i op) seed . ir re i xs

-- | Output stream that prints each entry (expression)
printStream :: [E (T K)] -> IO ()
printStream = traverse_ print

foldWithCtx :: RurePtr -> Int
            -> E (T K)
            -> E (T K)
            -> E (T K)
            -> [BS.ByteString]
            -> E (T K)
foldWithCtx re i op seed streamExpr = foldl' (applyOp i op) seed . ir re i streamExpr

runJac :: RurePtr -- ^ Record separator
       -> Int
       -> Program (T K)
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac re i e = fileProcessor re i (closedProgram i e)

-- evaluate something that has a fold nested in it
eWith :: RurePtr -> Int -> E (T K) -> [BS.ByteString] -> E (T K)
eWith re i (EApp _ (EApp _ (EApp _ (TBuiltin (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyStream) _) _))) Fold) op) seed) stream) = foldWithCtx re i op seed stream
eWith re i (EApp ty e0 e1)                                             = \bs -> eClosed i (EApp ty (eWith re i e0 bs) (eWith re i e1 bs))
eWith _ _ e@BBuiltin{}                                                 = const e
eWith _ _ e@UBuiltin{}                                                 = const e
eWith _ _ e@TBuiltin{}                                                 = const e
eWith _ _ e@StrLit{}                                                   = const e
eWith _ _ e@FloatLit{}                                                 = const e
eWith _ _ e@IntLit{}                                                   = const e
eWith _ _ e@BoolLit{}                                                  = const e
eWith re i (Tup ty es)                                                 = \bs -> Tup ty ((\e -> eWith re i e bs) <$> es)

-- TODO: passing in 'i' separately to each eClosed is sketch but... hopefully
-- won't blow up in our faces
--
-- | Given an expression, turn it into a function which will process the file.
fileProcessor :: RurePtr
              -> Int
              -> E (T K)
              -> Either StreamError ([BS.ByteString] -> IO ())
fileProcessor _ _ AllField{}    = Left NakedField
fileProcessor _ _ Field{}       = Left NakedField
fileProcessor _ _ (NBuiltin _ Ix) = Left NakedField
fileProcessor _ _ AllColumn{} = Right $ \inp ->
    printStream $ fmap mkStr inp
fileProcessor re _ (Column _ i) = Right $ \inp -> do
    printStream $ fmap (mkStr . atField re i) inp
fileProcessor re _ (IParseCol _ i) = Right $ \inp -> do
    printStream $ fmap (parseAsEInt . atField re i) inp
fileProcessor re _ (FParseCol _ i) = Right $ \inp -> do
    printStream $ fmap (parseAsF . atField re i) inp
-- TODO: this should extract any regex and compile them, use io/low-level API...
fileProcessor re i e@Guarded{} = Right $ \inp -> do
    printStream $ ir re i e inp
fileProcessor re i e@Implicit{} = Right $ \inp -> do
    printStream $ ir re i e inp
fileProcessor re i e@(EApp _ (EApp _ (BBuiltin _ Filter) _) _) = Right $ \inp -> do
    printStream $ ir re i e inp
fileProcessor re i e@(EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyStream) _))) Map) _) _) = Right $ \inp -> do
    printStream $ ir re i e inp
fileProcessor re i e@(EApp _ (EApp _ (BBuiltin _ Prior) _) _) = Right $ \inp -> do
    printStream $ ir re i e inp
fileProcessor re i e@(EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) _) _) _) = Right $ \inp -> do
    printStream $ ir re i e inp
fileProcessor re i e@(EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) _) _) _) = Right $ \inp -> do
    printStream $ ir re i e inp
fileProcessor _ _ Var{} = error "Internal error?"
fileProcessor _ _ e@IntLit{} = Right $ const (print e)
fileProcessor _ _ e@BoolLit{} = Right $ const (print e)
fileProcessor _ _ e@StrLit{} = Right $ const (print e)
fileProcessor _ _ e@FloatLit{} = Right $ const (print e)
fileProcessor _ _ e@RegexLit{} = Right $ const (print e)
fileProcessor _ _ Lam{} = Left UnevalFun
fileProcessor _ _ Dfn{} = badSugar
fileProcessor _ _ ResVar{} = badSugar
fileProcessor _ _ BBuiltin{} = Left UnevalFun
fileProcessor _ _ UBuiltin{} = Left UnevalFun
fileProcessor _ _ TBuiltin{} = Left UnevalFun
fileProcessor re i e = Right $ print . eWith re i e
