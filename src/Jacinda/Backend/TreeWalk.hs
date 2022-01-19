-- | Tree-walking interpreter
module Jacinda.Backend.TreeWalk ( runJac
                                ) where

-- TODO: normalize before mapping?

import           Control.Exception         (Exception, throw)
import           Control.Recursion         (cata, embed)
import qualified Data.ByteString           as BS
import           Data.Containers.ListUtils (nubOrdOn)
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

type FileBS = BS.ByteString

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

withFp :: FileBS -> E (T K) -> E (T K)
withFp fp = cata a where
    a (NBuiltinF _ Fp) = mkStr fp
    a x                = embed x

-- eval
eEval :: (FileBS, Int, BS.ByteString, V.Vector BS.ByteString) -- ^ Field context (for that line)
      -> E (T K)
      -> E (T K)
eEval (fp, ix, line, ctx) = go where
    go b@BoolLit{} = b
    go i@IntLit{} = i
    go f@FloatLit{} = f
    go str@StrLit{} = str
    go rr@RegexLit{} = rr
    go reϵ@RegexCompiled{} = reϵ
    go op@BBuiltin{} = op
    go op@UBuiltin{} = op
    go op@TBuiltin{} = op
    go (NBuiltin _ Nf) = mkI (fromIntegral $ V.length ctx)
    go (EApp ty op@BBuiltin{} e) = EApp ty op (go e)
    go (NBuiltin _ Ix) = mkI (fromIntegral ix)
    go (NBuiltin _ Fp) = mkStr fp
    go AllField{} = StrLit tyStr line
    go (Field _ i) = StrLit tyStr (ctx ! (i-1)) -- cause vector indexing starts at 0
    go (EApp _ (UBuiltin _ IParse) e) =
        let eI = asStr (go e)
            in parseAsEInt eI
    go (EApp _ (UBuiltin _ FParse) e) =
        let eI = asStr (go e)
            in parseAsF eI
    go (EApp _ (UBuiltin (TyArr _ _ (TyB _ TyInteger)) Parse) e) =
        let eI = asStr (go e)
            in parseAsEInt eI
    go (EApp _ (UBuiltin (TyArr _ _ (TyB _ TyFloat)) Parse) e) =
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
            -- TODO: copy??
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

applyOp :: E (T K) -- ^ Operator
        -> E (T K)
        -> E (T K)
        -> E (T K)
applyOp op e e' = eClosed undefined (EApp undefined (EApp undefined op e) e') -- FIXME: undefined is ??

atField :: RurePtr
        -> Int
        -> BS.ByteString -- ^ Line
        -> BS.ByteString
atField re i = (! (i-1)) . splitBy re

mkCtx :: FileBS -> RurePtr -> Int -> BS.ByteString -> (FileBS, Int, BS.ByteString, V.Vector BS.ByteString)
mkCtx fp re ix line = (fp, ix, line, splitBy re line)

applyUn :: E (T K)
        -> E (T K)
        -> E (T K)
applyUn unOp e =
    case eLoc unOp of
        TyArr _ _ res -> eClosed undefined (EApp res unOp e)
        _             -> error "Internal error?"

-- | Turn an expression representing a stream into a stream of expressions (using line as context)
ir :: FileBS
   -> RurePtr
   -> E (T K)
   -> [BS.ByteString]
   -> [E (T K)] -- TODO: include chunks/context too?
ir _ _ AllColumn{} = fmap mkStr
ir _ re (Column _ i) = fmap (mkStr . atField re i)
ir _ re (IParseCol _ i) = fmap (parseAsEInt . atField re i)
ir _ re (FParseCol _ i) = fmap (parseAsF . atField re i)
ir fp re (Implicit _ e) =
    let e' = compileR e
        in imap (\ix line -> eEval (mkCtx fp re ix line) e')
ir fp re (Guarded _ pe e) =
    let pe' = compileR pe
        e' = compileR e
    -- FIXME: compile e too?
    -- TODO: normalize before stream
        in imap (\ix line -> eEval (mkCtx fp re ix line) e') . ifilter (\ix line -> asBool (eEval (mkCtx fp re ix line) pe'))
ir fp re (EApp _ (EApp _ (BBuiltin _ Map) op) stream) = let op' = compileR (withFp fp op) in fmap (applyUn op') . ir fp re stream
ir fp re (EApp _ (EApp _ (BBuiltin _ Filter) op) stream) =
    let op' = compileR (withFp fp op)
        in filter (asBool . applyUn op') . ir fp re stream
ir fp re (EApp _ (EApp _ (BBuiltin _ Prior) op) stream) = prior (applyOp (withFp fp op)) . ir fp re stream
ir fp re (EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) op) streaml) streamr) = \lineStream ->
    let
        irl = ir fp re streaml lineStream
        irr = ir fp re streamr lineStream
    in zipWith (applyOp (withFp fp op)) irl irr
ir fp re (EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) op) seed) xs) =
    scanl' (applyOp (withFp fp op)) seed . ir fp re xs
ir fp re (EApp _ (UBuiltin (TyArr _ (TyApp _ _ (TyB _ TyStr)) _) Dedup) e) =
    nubOrdOn asStr . ir fp re e
ir fp re (EApp _ (UBuiltin (TyArr _ (TyApp _ _ (TyB _ TyInteger)) _) Dedup) e) =
    nubOrdOn asInt . ir fp re e
ir fp re (EApp _ (UBuiltin (TyArr _ (TyApp _ _ (TyB _ TyFloat)) _) Dedup) e) =
    nubOrdOn asFloat . ir fp re e
ir fp re (EApp _ (UBuiltin (TyArr _ (TyApp _ _ (TyB _ TyBool)) _) Dedup) e) =
    nubOrdOn asBool . ir fp re e

-- | Output stream that prints each entry (expression)
printStream :: [E (T K)] -> IO ()
printStream = traverse_ print

foldWithCtx :: FileBS
            -> RurePtr
            -> E (T K)
            -> E (T K)
            -> E (T K)
            -> [BS.ByteString]
            -> E (T K)
foldWithCtx fp re op seed streamExpr = foldl' (applyOp op) seed . ir fp re streamExpr

runJac :: FileBS
       -> RurePtr -- ^ Record separator
       -> Int
       -> Program (T K)
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac fp re i e = fileProcessor fp re (closedProgram i e)

-- evaluate something that has a fold nested in it
eWith :: FileBS -> RurePtr -> E (T K) -> [BS.ByteString] -> E (T K)
eWith fp re (EApp _ (EApp _ (EApp _ (TBuiltin (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyStream) _) _))) Fold) op) seed) stream) = foldWithCtx fp re op seed stream
eWith fp re (EApp ty e0 e1)                                                                                                            = \bs -> eClosed undefined (EApp ty (eWith fp re e0 bs) (eWith fp re e1 bs))
eWith _ _ e@BBuiltin{}                                                                                                                = const e
eWith _ _ e@UBuiltin{}                                                                                                                = const e
eWith _ _ e@TBuiltin{}                                                                                                                = const e
eWith _ _ e@StrLit{}                                                                                                                  = const e
eWith _ _ e@FloatLit{}                                                                                                                = const e
eWith _ _ e@IntLit{}                                                                                                                  = const e
eWith _ _ e@BoolLit{}                                                                                                                 = const e
eWith fp re (Tup ty es)                                                                                                                = \bs -> Tup ty ((\e -> eWith fp re e bs) <$> es)
eWith fp re (OptionVal ty e)                                                                                                           = \bs -> OptionVal ty ((\eϵ -> eWith fp re eϵ bs) <$> e)
-- TODO: rewrite tuple-of-folds as fold-of-tuples ... "compile" to E (T K) -> E (T K)
-- OR "compile" to [(Int, E (T K)] -> ...

-- | Given an expression, turn it into a function which will process the file.
fileProcessor :: FileBS
              -> RurePtr
              -> E (T K)
              -> Either StreamError ([BS.ByteString] -> IO ())
fileProcessor _ _ AllField{}    = Left NakedField
fileProcessor _ _ Field{}       = Left NakedField
fileProcessor _ _ (NBuiltin _ Ix) = Left NakedField
fileProcessor _ _ AllColumn{} = Right $ \inp ->
    printStream $ fmap mkStr inp
fileProcessor _ re (Column _ i) = Right $ \inp -> do
    printStream $ fmap (mkStr . atField re i) inp
fileProcessor _ re (IParseCol _ i) = Right $ \inp -> do
    printStream $ fmap (parseAsEInt . atField re i) inp
fileProcessor _ re (FParseCol _ i) = Right $ \inp -> do
    printStream $ fmap (parseAsF . atField re i) inp
fileProcessor fp re e@Guarded{} = Right $ \inp ->
    printStream $ ir fp re e inp
fileProcessor fp re e@Implicit{} = Right $ \inp ->
    printStream $ ir fp re e inp
fileProcessor fp re e@(EApp _ (EApp _ (BBuiltin _ Filter) _) _) = Right $ \inp ->
    printStream $ ir fp re e inp
fileProcessor fp re e@(EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyStream) _))) Map) _) _) = Right $ \inp ->
    printStream $ ir fp re e inp
fileProcessor fp re e@(EApp _ (EApp _ (BBuiltin _ Prior) _) _) = Right $ \inp ->
    printStream $ ir fp re e inp
fileProcessor fp re e@(EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) _) _) _) = Right $ \inp ->
    printStream $ ir fp re e inp
fileProcessor fp re e@(EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) _) _) _) = Right $ \inp ->
    printStream $ ir fp re e inp
fileProcessor fp re e@(EApp _ (UBuiltin _ Dedup) _) = Right $ \inp ->
    printStream $ ir fp re e inp
fileProcessor _ _ Var{} = error "Internal error?"
fileProcessor _ _ e@IntLit{} = Right $ const (print e)
fileProcessor _ _ e@BoolLit{} = Right $ const (print e)
fileProcessor _ _ e@StrLit{} = Right $ const (print e)
fileProcessor _ _ e@FloatLit{} = Right $ const (print e)
fileProcessor _ _ e@RegexLit{} = Right $ const (print e)
fileProcessor fp _ (NBuiltin _ Fp) = Right $ const (print fp)
fileProcessor _ _ Lam{} = Left UnevalFun
fileProcessor _ _ Dfn{} = badSugar
fileProcessor _ _ ResVar{} = badSugar
fileProcessor _ _ BBuiltin{} = Left UnevalFun
fileProcessor _ _ UBuiltin{} = Left UnevalFun
fileProcessor _ _ TBuiltin{} = Left UnevalFun
fileProcessor fp re e = Right $ print . eWith fp re e
