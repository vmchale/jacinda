{-# LANGUAGE OverloadedStrings #-}

-- | Tree-walking interpreter
module Jacinda.Backend.TreeWalk ( runJac
                                ) where

-- TODO: normalize before mapping?

import           Control.Exception          (Exception, throw)
import           Control.Monad.State.Strict (State, get, modify, runState)
import           Data.Bifunctor             (bimap)
import qualified Data.ByteString            as BS
import           Data.Containers.ListUtils  (nubIntOn, nubOrdOn)
import           Data.Foldable              (foldl', traverse_)
import qualified Data.IntMap                as IM
import           Data.List                  (scanl', transpose, unzip4)
import           Data.List.Ext
import           Data.Maybe                 (mapMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Vector                as V
import           Intern.Name                (Name (Name))
import           Intern.Unique              (Unique (Unique))
import           Jacinda.AST
import           Jacinda.Backend.Normalize
import           Jacinda.Backend.Printf
import           Jacinda.Regex              (captures', find', findCapture, isMatch', splitBy, substr)
import           Jacinda.Ty.Const
import           Regex.Rure                 (RurePtr)
import           System.IO                  (hFlush, stdout)

data StreamError = NakedField
                 | UnevalFun
                 | TupOfStreams -- ^ Reject a tuple of streams
                 | BadCtx
                 | InternalError
                 deriving (Show)

instance Exception StreamError where

(!) :: V.Vector a -> Int -> a
v ! ix = case v V.!? ix of
    Just x  -> x
    Nothing -> throw $ IndexOutOfBounds ix

noRes :: E b -> String -> a
noRes e ty = error ("Internal error: " ++ show e ++ " did not normalize to appropriate type, expected " ++ ty)

badSugar :: a
badSugar = error "Internal error: dfn syntactic sugar at a stage where it should not be."

asInt :: E a -> Integer
asInt (IntLit _ i) = i
asInt e            = noRes e "Int"

asBool :: E a -> Bool
asBool (BoolLit _ b) = b
asBool e             = noRes e "Bool"

asStr :: E a -> BS.ByteString
asStr (StrLit _ str) = str
asStr e              = noRes e "Str"

asFloat :: E a -> Double
asFloat (FloatLit _ f) = f
asFloat e              = noRes e "Float"

asRegex :: E a -> RurePtr
asRegex (RegexCompiled re) = re
asRegex e                  = noRes e "Regex"

asArr :: E a -> V.Vector (E a)
asArr (Arr _ es) = es
asArr e          = noRes e "List"

asOpt :: E a -> Maybe (E a)
asOpt (OptionVal _ e) = e
asOpt e               = noRes e "Option"

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
    go (NBuiltin _ Nf) = mkI (fromIntegral $ V.length ctx)
    go (EApp ty op@BBuiltin{} e) = EApp ty op (go e)
    go (NBuiltin _ Ix) = mkI (fromIntegral ix)
    go (NBuiltin _ None) = OptionVal undefined Nothing
    go (EApp ty (UBuiltin _ Some) e) =
        let eI = go e
            in OptionVal ty (Just eI)
    go AllField{} = StrLit tyStr line
    go (Field _ i) = StrLit tyStr (ctx ! (i-1)) -- cause vector indexing starts at 0
    go LastField{} = StrLit tyStr (V.last ctx)
    go (EApp _ (UBuiltin _ IParse) e) =
        let eI = asStr (go e)
            in parseAsEInt eI
    go (EApp _ (UBuiltin (TyArr _ (TyB _ TyInteger) _) Negate) e) =
        let eI = asInt (go e)
            in mkI (negate eI)
    go (EApp _ (UBuiltin (TyArr _ (TyB _ TyFloat) _) Negate) e) =
        let eI = asFloat (go e)
            in mkF (negate eI)
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
            (StrLit _ strϵ, RegexCompiled reϵ) -> BoolLit tyB (isMatch' reϵ strϵ)
            (StrLit{}, _)                      -> noRes eI' "Regex"
            _                                  -> noRes eI "Str"
    go (EApp _ (EApp _ (BBuiltin _ NotMatches) e) e') =
        let eI = go e
            eI' = go e'
        in case (eI, eI') of
            (StrLit _ strϵ, RegexCompiled reϵ) -> BoolLit tyB (not $ isMatch' reϵ strϵ)
            (StrLit{}, _)                      -> noRes eI' "Regex"
            _                                  -> noRes eI "Str"
    go (EApp _ (EApp _ (BBuiltin _ Match) e) e') =
        let eI = asRegex (go e)
            eI' = asStr (go e')
        in asTup (find' eI eI')
    go (EApp _ (EApp _ (EApp _ (TBuiltin _ Captures) e0) e1) e2) =
        let e0' = asStr (go e0)
            e1' = asInt (go e1)
            e2' = asRegex (go e2)
            in OptionVal (tyOpt tyStr) (mkStr <$> findCapture e2' e0' (fromIntegral e1'))
    go (EApp _ (EApp _ (EApp _ (TBuiltin _ AllCaptures) e0) e1) e2) =
        let e0' = asStr (go e0)
            e1' = asInt (go e1)
            e2' = asRegex (go e2)
            in Arr (mkVec tyStr) (mkStr <$> V.fromList (captures' e2' e0' (fromIntegral e1')))
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
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Exp) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in mkI (eI ^ eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Plus) e) e') =
        let eI = asStr (go e)
            eI' = asStr (go e')
            -- TODO: copy??
            in mkStr (eI <> eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Eq) e) e') =
        let eI = asStr (go e)
            eI' = asStr (go e')
            in BoolLit tyB (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Gt) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyB (eI > eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Lt) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyB (eI < eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Eq) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyB (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Neq) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyB (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Neq) e) e') =
        let eI = asStr (go e)
            eI' = asStr (go e')
            in BoolLit tyB (eI /= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Leq) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyB (eI <= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Geq) e) e') =
        let eI = asInt (go e)
            eI' = asInt (go e')
            in BoolLit tyB (eI <= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Eq) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in BoolLit tyB (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Neq) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in BoolLit tyB (eI /= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Lt) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in BoolLit tyB (eI < eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Gt) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in BoolLit tyB (eI > eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Geq) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in BoolLit tyB (eI >= eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Leq) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in BoolLit tyB (eI <= eI')
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
            in mkF (eI * eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Exp) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in mkF (eI ** eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyBool) _) Eq) e) e') =
        let eI = asBool (go e)
            eI' = asBool (go e')
            in BoolLit tyB (eI == eI')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyBool) _) Neq) e) e') =
        let eI = asBool (go e)
            eI' = asBool (go e')
            in BoolLit tyB (eI /= eI')
    go (EApp _ (EApp _ (BBuiltin _ Div) e) e') =
        let eI = asFloat (go e)
            eI' = asFloat (go e')
            in FloatLit tyF (eI / eI')
    go (EApp _ (EApp _ (BBuiltin _ And) e) e') =
        let b = asBool (go e)
            b' = asBool (go e')
            in BoolLit tyB (b && b')
    go (EApp _ (EApp _ (BBuiltin _ Or) e) e') =
        let b = asBool e
            b' = asBool e'
            in BoolLit tyB (b || b')
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
            in Arr undefined (mkStr <$> bss)
    go (EApp _ (EApp _ (BBuiltin _ Splitc) e) e') =
        let str = asStr (go e)
            c = the (asStr (go e'))
            bss = BS.split c str
            in Arr undefined (mkStr <$> V.fromList bss)
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
        in BoolLit tyB (not eI)
    go (EApp _ (UBuiltin _ (At i)) e) =
        let eI = go e
            in case eI of
                (Arr _ es) -> go (es V.! (i-1))
                _          -> noRes eI "List"
    go (EApp _ (UBuiltin _ (Select i)) e) =
        let eI = go e
            in case eI of
                (Tup _ es) -> go (es !! (i-1))
                _          -> noRes eI "Tuple"
    go (EApp _ (EApp _ (BBuiltin _ Sprintf) e) e') =
        let eI = asStr (go e)
            eI' = go e'
        in mkStr (sprintf eI eI')
    go (OptionVal ty e) =
        OptionVal ty (go <$> e)
    go (EApp _ (EApp _ (EApp _ (TBuiltin _ Option) e0) e1) e2) =
        let e0' = go e0
            e1' = go e1
            e2' = go e2
        in case asOpt e2' of
                Nothing -> e0'
                Just e  -> go (EApp undefined e1' e)
    go (EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyVec) _))) Map) x) y) =
        let x' = go x
            y' = asArr (go y)
        in Arr undefined (applyUn' x' <$> y')
        where applyUn' :: E (T K) -> E (T K) -> E (T K)
              applyUn' e e' = go (EApp undefined e e')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyOption) _))) Map) x) y) =
        let x' = go x
            y' = asOpt (go y)
        in OptionVal undefined (applyUn' x' <$> y')
        where applyUn' :: E (T K) -> E (T K) -> E (T K)
              applyUn' e e' = go (EApp undefined e e')
    go (EApp _ (EApp _ (EApp _ (TBuiltin (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyVec) _) _))) Fold) f) seed) xs) =
        let f' = go f
            seed' = go seed
            xs' = asArr (go xs)
        in foldE f' seed' xs'
        where foldE op = V.foldl' (applyOp' op)
              applyOp' op e e' = go (EApp undefined (EApp undefined op e) e')
    go (EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyVec) _) _)) Fold1) f) xs) =
        let f' = go f
            xs' = asArr (go xs)
        in
            case V.uncons xs' of
                Just (y, ys) -> foldE f' y ys
                Nothing      -> throw EmptyFold
        where foldE op = V.foldl' (applyOp' op)
              applyOp' op e e' = go (EApp undefined (EApp undefined op e) e')
    go (Arr ty es) = Arr ty (go <$> es)
    go (Cond _ p e0 e1) =
        let p' = asBool (go p)
            in if p' then go e0 else go e1
    go (EApp _ (UBuiltin _ TallyList) e) =
        let xs = asArr (go e)
            in mkI $ fromIntegral $ V.length xs
    go e = error ("Internal error: " ++ show e)

-- just shove some big number into the renamer and hope it doesn't clash (bad,
-- hack, this is why we got kicked out of the garden of Eden)
reprehensible :: Int
reprehensible = (maxBound :: Int) `div` 2

applyOp :: E (T K) -- ^ Operator
        -> E (T K)
        -> E (T K)
        -> E (T K)
applyOp op e e' = eClosed reprehensible (EApp undefined (EApp undefined op e) e') -- FIXME: undefined is ??

atField :: RurePtr
        -> Int
        -> BS.ByteString -- ^ Line
        -> BS.ByteString
atField re i = (! (i-1)) . splitBy re

mkCtx :: RurePtr -> Int -> BS.ByteString -> (Int, BS.ByteString, V.Vector BS.ByteString)
mkCtx re ix line = (ix, line, splitBy re line)

applyUn :: E (T K)
        -> E (T K)
        -> E (T K)
applyUn unOp e =
    case eLoc unOp of
        TyArr _ _ res -> eClosed reprehensible (EApp res unOp e)
        _             -> error "Internal error?"

-- | Turn an expression representing a stream into a stream of expressions (using line as context)
ir :: RurePtr
   -> E (T K)
   -> [BS.ByteString]
   -> [E (T K)] -- TODO: include chunks/context too?
ir _ AllColumn{} = fmap mkStr
ir re (Column _ i) = fmap (mkStr . atField re i)
ir re (IParseCol _ i) = fmap (parseAsEInt . atField re i)
ir re (FParseCol _ i) = fmap (parseAsF . atField re i)
ir re (ParseCol ty@(TyApp _ _ (TyB _ TyFloat)) i) = ir re (FParseCol ty i)
ir re (ParseCol ty@(TyApp _ _ (TyB _ TyInteger)) i) = ir re (IParseCol ty i)
ir re (Implicit _ e) =
    imap (\ix line -> eEval (mkCtx re ix line) e)
ir re (Guarded _ pe e) =
    -- TODO: normalize before stream
    fmap (\(ix, line) -> eEval (mkCtx re ix line) e) . ifilter' (\ix line -> asBool (eEval (mkCtx re ix line) pe))
ir re (EApp _ (EApp _ (BBuiltin _ Map) op) stream) = fmap (applyUn op) . ir re stream
ir re (EApp _ (EApp _ (BBuiltin _ Filter) op) stream) =
    filter (asBool . applyUn op) . ir re stream
ir re (EApp _ (EApp _ (BBuiltin _ MapMaybe) op) stream) =
    mapMaybe (asOpt . applyUn op) . ir re stream
ir re (EApp _ (UBuiltin _ CatMaybes) stream) =
    mapMaybe asOpt . ir re stream
ir re (EApp _ (EApp _ (BBuiltin _ Prior) op) stream) = prior (applyOp op) . ir re stream
ir re (EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) op) streaml) streamr) = \lineStream ->
    let
        irl = ir re streaml lineStream
        irr = ir re streamr lineStream
    in zipWith (applyOp op) irl irr
ir re (EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) op) seed) xs) =
    scanl' (applyOp op) seed . ir re xs
ir re (EApp _ (EApp _ (BBuiltin (TyArr _ (TyArr _ _ (TyB _ TyStr)) _) DedupOn) op) e) =
    nubOrdOn (asStr . applyUn op) . ir re e
ir re (EApp _ (UBuiltin (TyArr _ (TyApp _ _ (TyB _ TyStr)) _) Dedup) e) =
    nubOrdOn asStr . ir re e
ir re (EApp _ (UBuiltin (TyArr _ (TyApp _ _ (TyB _ TyInteger)) _) Dedup) e) =
    nubIntOn (fromIntegral . asInt) . ir re e
ir re (EApp _ (UBuiltin (TyArr _ (TyApp _ _ (TyB _ TyFloat)) _) Dedup) e) =
    nubIntOn (fromEnum . asFloat) . ir re e
ir re (EApp _ (UBuiltin (TyArr _ (TyApp _ _ (TyB _ TyBool)) _) Dedup) e) =
    nubIntOn (fromEnum . asBool) . ir re e

-- | Output stream that prints each entry (expression)
printStream :: Bool -- ^ Flush?
            -> [E (T K)] -> IO ()
printStream False = traverse_ print
printStream True = traverse_ ((*> fflush) . print)
    where fflush = hFlush stdout

foldWithCtx :: RurePtr
            -> E (T K)
            -> E (T K)
            -> E (T K)
            -> [BS.ByteString]
            -> E (T K)
foldWithCtx re op seed streamExpr = foldl' (applyOp op) seed . ir re streamExpr

fold1 :: RurePtr
      -> E (T K)
      -> E (T K)
      -> [BS.ByteString]
      -> E (T K)
fold1 re op streamExpr bs =
    case ir re streamExpr bs of
        e:es -> foldl' (applyOp op) e es
        _    -> throw EmptyFold

runJac :: RurePtr -- ^ Record separator
       -> Int
       -> Program (T K)
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac re i e = fileProcessor re (flushD e) (closedProgram i e)

foldAll :: RurePtr
        -> [(Int, E (T K), E (T K), E (T K))]
        -> [BS.ByteString]
        -> [(Int, E (T K))]
foldAll re foldExprs bs = evalAll seeds (mkStreams streamExprs) where
    (is, ops, seeds, streamExprs) = unzip4 foldExprs
    mkStreams = fmap (\streamExpr -> ir re streamExpr bs)

    evalAll seedsϵ ess | not (any null ess) = let es' = zipWith3 applyOp' ops seedsϵ (headMaybe <$> ess) in es' `seqAll` evalAll es' (tail' <$> ess)
                       -- if I try to use the (all null ess) criterion it space
                       -- leaks like crazy so... inspect only when we need?
                       --
                       -- (still leaks space... but less)
                       | not (all null ess) = let es' = zipWith3 applyOp' ops seedsϵ (headMaybe <$> ess) in es' `seqAll` evalAll es' (tail' <$> ess)
                       | otherwise = zip is seedsϵ

    seqAll (e:es) z = foldr seq e es `seq` z
    seqAll [] z     = z

    applyOp' op seed (Just e) = applyOp op seed e
    applyOp' _ seed Nothing   = seed

    headMaybe []    = Nothing
    headMaybe (x:_) = Just x

    tail' []     = []
    tail' (_:xs) = xs

ungather :: IM.IntMap (E (T K)) -> E (T K) -> E (T K)
ungather st (Var _ (Name _ (Unique i) _)) =
    case IM.lookup i st of
        Just res -> res
        Nothing  -> throw InternalError
ungather st (EApp ty e0 e1)  = EApp ty (ungather st e0) (ungather st e1)
ungather st (Tup ty es)      = Tup ty (ungather st <$> es)
ungather st (Arr ty es)      = Arr ty (ungather st <$> es)
ungather st (OptionVal ty e) = OptionVal ty (ungather st <$> e)
ungather _ e@BBuiltin{}      = e
ungather _ e@UBuiltin{}      = e
ungather _ (NBuiltin _ None) = OptionVal undefined Nothing
ungather _ e@NBuiltin{}      = e
ungather _ e@TBuiltin{}      = e
ungather _ e@StrLit{}        = e
ungather _ e@BoolLit{}       = e
ungather _ e@FloatLit{}      = e
ungather _ e@IntLit{}        = e
ungather _ e@RegexCompiled{} = e

mkFoldVar :: Int -> b -> E b
mkFoldVar i l = Var l (Name "fold_placeholder" (Unique i) l)

gatherFoldsM :: E (T K) -> State (Int, [(Int, E (T K), E (T K), E (T K))]) (E (T K))
gatherFoldsM (EApp _ (EApp _ (EApp _ (TBuiltin (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyStream) _) _))) Fold) op) seed) stream) = do
    (i,_) <- get
    modify (bimap (+1) ((i, op, seed, stream) :))
    pure $ mkFoldVar i undefined
gatherFoldsM (EApp ty e0 e1) = EApp ty <$> gatherFoldsM e0 <*> gatherFoldsM e1
gatherFoldsM (Tup ty es) = Tup ty <$> traverse gatherFoldsM es
gatherFoldsM (Arr ty es) = Arr ty <$> traverse gatherFoldsM es
gatherFoldsM (OptionVal ty e) = OptionVal ty <$> traverse gatherFoldsM e
gatherFoldsM (Cond ty p e e') = Cond ty <$> gatherFoldsM p <*> gatherFoldsM e <*> gatherFoldsM e'
-- gatherFoldsM (Lam t n e) = Lam t n <$> gatherFoldsM e
gatherFoldsM (NBuiltin _ None) = pure $ OptionVal undefined Nothing
gatherFoldsM e@BBuiltin{} = pure e
gatherFoldsM e@TBuiltin{} = pure e
gatherFoldsM e@UBuiltin{} = pure e
gatherFoldsM e@NBuiltin{} = pure e
gatherFoldsM e@StrLit{} = pure e
gatherFoldsM e@FloatLit{} = pure e
gatherFoldsM e@IntLit{} = pure e
gatherFoldsM e@BoolLit{} = pure e
gatherFoldsM e@RegexCompiled{} = pure e

eWith :: RurePtr -> E (T K) -> [BS.ByteString] -> E (T K)
eWith re (EApp _ (EApp _ (EApp _ (TBuiltin (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyStream) _) _))) Fold) op) seed) stream) = foldWithCtx re op seed stream
eWith re (EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyStream) _) _)) Fold1) op) stream)                          = fold1 re op stream
-- TODO: function APPLIED to fold/fold1 res.
eWith _ e@BBuiltin{}                                                                                                                 = const e
eWith _ e@UBuiltin{}                                                                                                                 = const e
eWith _ e@TBuiltin{}                                                                                                                 = const e
eWith _ e@StrLit{}                                                                                                                   = const e
eWith _ e@FloatLit{}                                                                                                                 = const e
eWith _ e@IntLit{}                                                                                                                   = const e
eWith _ e@BoolLit{}                                                                                                                  = const e
eWith re e = \bs ->
    let (eHoles, (_, folds)) = runState (gatherFoldsM e) (0, []) -- 0 state, should contain no vars by now
        in eClosed undefined $ ungather (IM.fromList $ foldAll re folds bs) eHoles

takeConcatMap :: (a -> [b]) -> [a] -> [b]
takeConcatMap f = concat . transpose . fmap f

-- | Given an expression, turn it into a function which will process the file.
fileProcessor :: RurePtr
              -> Bool -- ^ Flush output?
              -> E (T K)
              -> Either StreamError ([BS.ByteString] -> IO ())
fileProcessor _ _ AllField{}    = Left NakedField
fileProcessor _ _ Field{}       = Left NakedField
fileProcessor _ _ (NBuiltin _ Ix) = Left NakedField
fileProcessor re f e@AllColumn{} = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@Column{} = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@IParseCol{} = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@FParseCol{} = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@ParseCol{} = Right $ \inp -> printStream f $ ir re e inp
fileProcessor re f e@Guarded{} = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@Implicit{} = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@(EApp _ (EApp _ (BBuiltin _ Filter) _) _) = Right $ \inp ->
    printStream f $ ir re e inp
-- at the moment, catMaybes only works on streams
fileProcessor re f e@(EApp _ (UBuiltin _ CatMaybes) _) = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@(EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyStream) _))) Map) _) _) = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@(EApp _ (EApp _ (BBuiltin (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyStream) _))) MapMaybe) _) _) = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@(EApp _ (EApp _ (BBuiltin _ Prior) _) _) = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@(EApp _ (EApp _ (BBuiltin _ DedupOn) _) _) = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@(EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) _) _) _) = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@(EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) _) _) _) = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f e@(EApp _ (UBuiltin _ Dedup) _) = Right $ \inp ->
    printStream f $ ir re e inp
fileProcessor re f (Anchor _ es) = Right $ \inp ->
    printStream f $ takeConcatMap (\e -> ir re e inp) es
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
fileProcessor re _ e = Right $ print . eWith re e
