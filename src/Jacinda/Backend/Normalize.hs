{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Backend.Normalize ( eClosed
                                 , closedProgram
                                 , readFloat
                                 , mkI
                                 , mkF
                                 , mkStr
                                 , parseAsEInt
                                 , parseAsF
                                 , the
                                 , asTup
                                 , EvalError (..)
                                 -- * Monad
                                 , runEvalM
                                 , eNorm
                                 ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as ASCII
import           Data.Foldable              (traverse_)
import qualified Data.IntMap                as IM
import           Data.Semigroup             ((<>))
import qualified Data.Vector                as V
import           Data.Word                  (Word8)
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST
import           Jacinda.Backend.Parse
import           Jacinda.Backend.Printf
import           Jacinda.Regex
import           Jacinda.Rename
import           Jacinda.Ty.Const
import           Regex.Rure                 (RureMatch (..))

data EvalError = EmptyFold
               | IndexOutOfBounds Int
               deriving (Show)

instance Exception EvalError where

mkI :: Integer -> E (T K)
mkI = IntLit tyI

mkF :: Double -> E (T K)
mkF = FloatLit tyF

mkStr :: BS.ByteString -> E (T K)
mkStr = StrLit tyStr

parseAsEInt :: BS.ByteString -> E (T K)
parseAsEInt = mkI . readDigits

parseAsF :: BS.ByteString -> E (T K)
parseAsF = FloatLit tyF . readFloat

the :: BS.ByteString -> Word8
the bs = case BS.uncons bs of
    Nothing     -> error "Empty splitc char!"
    Just (c,"") -> c
    Just _      -> error "Splitc takes only one char!"

readFloat :: BS.ByteString -> Double
readFloat = read . ASCII.unpack -- TODO: readMaybe

desugar :: a
desugar = error "Should have been desugared by this stage."

data LetCtx = LetCtx { binds    :: IM.IntMap (E (T K))
                     , renames_ :: Renames
                     }

instance HasRenames LetCtx where
    rename f s = fmap (\x -> s { renames_ = x }) (f (renames_ s))

mapBinds :: (IM.IntMap (E (T K)) -> IM.IntMap (E (T K))) -> LetCtx -> LetCtx
mapBinds f (LetCtx b r) = LetCtx (f b) r

type EvalM = State LetCtx

mkLetCtx :: Int -> LetCtx
mkLetCtx i = LetCtx IM.empty (Renames i IM.empty)

runEvalM :: Int
         -> EvalM a
         -> a
runEvalM i = flip evalState (mkLetCtx i)

eClosed :: Int
        -> E (T K)
        -> E (T K)
eClosed i = runEvalM i . eNorm

closedProgram :: Int
              -> Program (T K)
              -> E (T K)
closedProgram i (Program ds e) = runEvalM i $
    traverse_ processDecl ds *> eNorm e

processDecl :: D (T K)
            -> EvalM ()
processDecl (FunDecl (Nm _ (U i) _) [] e) = do
    e' <- eNorm e
    modify (mapBinds (IM.insert i e'))
processDecl _ = pure ()

asTup :: Maybe RureMatch -> E (T K)
asTup Nothing                = OptionVal undefined Nothing
asTup (Just (RureMatch s e)) = OptionVal undefined (Just $ Tup undefined (mkI . fromIntegral <$> [s, e]))

-- don't need to rename op because it's being used in a map, can't affect etc.
applyUn :: E (T K)
        -> E (T K)
        -> EvalM (E (T K))
applyUn unOp e =
    case eLoc unOp of
        TyArr _ _ res -> eNorm (EApp res unOp e)
        _             -> error "Internal error?"

applyOp :: E (T K)
        -> E (T K)
        -> E (T K)
        -> EvalM (E (T K))
applyOp op@BB{}  e e' = eNorm (EApp undefined (EApp undefined op e) e') -- don't need rename if not a lambda
applyOp op e e'       = do {op' <- rE op ; eNorm (EApp undefined (EApp undefined op' e) e')}

foldE :: E (T K)
      -> E (T K)
      -> V.Vector (E (T K))
      -> EvalM (E (T K))
foldE op = V.foldM' (applyOp op)

-- TODO: equality on tuples, lists
eNorm :: E (T K)
      -> EvalM (E (T K))
eNorm e@Field{}       = pure e
eNorm e@IntLit{}      = pure e
eNorm e@FloatLit{}    = pure e
eNorm e@BoolLit{}     = pure e
eNorm e@StrLit{}      = pure e
eNorm e@RegexLit{}    = pure e
eNorm e@RegexCompiled{} = pure e
eNorm e@UB{}    = pure e
eNorm e@Column{}      = pure e
eNorm e@AllColumn{}   = pure e
eNorm e@IParseCol{}   = pure e
eNorm e@FParseCol{}   = pure e
eNorm e@ParseCol{}    = pure e
eNorm e@AllField{}    = pure e
eNorm e@LastField{}   = pure e
eNorm (Guarded ty pe e) = Guarded ty <$> eNorm pe <*> eNorm e
eNorm (Implicit ty e) = Implicit ty <$> eNorm e
eNorm (Lam ty n e)    = Lam ty n <$> eNorm e
eNorm e@BB{}    = pure e
eNorm e@TB{}    = pure e
eNorm (Tup tys es)    = Tup tys <$> traverse eNorm es
eNorm (Anchor ty es)  = Anchor ty <$> traverse eNorm es
eNorm (NB ty None) = pure $ OptionVal ty Nothing
eNorm e@NB{}    = pure e
eNorm (EApp ty op@BB{} e) = EApp ty op <$> eNorm e
eNorm (EApp ty (EApp ty' op@(BB _ Matches) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit _ str, RegexCompiled re) -> BoolLit tyB (isMatch' re str)
        _                                -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ NotMatches) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit _ str, RegexCompiled re) -> BoolLit tyB (not $ isMatch' re str)
        _                                -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB (TyArr _ (TyB _ TyInteger) _) Max) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> i `seq` j `seq` IntLit tyI (max i j)
        _                        -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB (TyArr _ (TyB _ TyInteger) _) Min) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> i `seq` j `seq` IntLit tyI (min i j)
        _                        -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB (TyArr _ (TyB _ TyFloat) _) Max) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ x, FloatLit _ y) -> x `seq` y `seq` FloatLit tyF (max x y)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB (TyArr _ (TyB _ TyFloat) _) Min) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ x, FloatLit _ y) -> x `seq` y `seq` FloatLit tyF (min x y)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Split) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit l str, RegexCompiled re) -> let bss = splitBy re str in Arr undefined (StrLit l <$> bss)
        _                                -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Splitc) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit l str, StrLit _ c) -> let bss = BS.split (the c) str in Arr undefined (StrLit l <$> V.fromList bss)
        _                          -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty op@(UB _ Floor) e) = do
    eI <- eNorm e
    pure $ case eI of
        (FloatLit _ f) -> mkI (floor f)
        _              -> EApp ty op eI
eNorm (EApp ty op@(UB _ Ceiling) e) = do
    eI <- eNorm e
    pure $ case eI of
        (FloatLit _ f) -> mkI (ceiling f)
        _              -> EApp ty op eI
eNorm (EApp ty0 (EApp ty1 op@(BB _ Minus) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> i `seq` j `seq` IntLit tyI (i-j)
        (FloatLit _ i, FloatLit _ j) -> i `seq` j `seq` FloatLit tyF (i-j)
        _                            -> EApp ty0 (EApp ty1 op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Times) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> i `seq` j `seq` IntLit tyI (i*j)
        (FloatLit _ i, FloatLit _ j) -> i `seq` j `seq` FloatLit tyF (i*j)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Exp) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> i `seq` j `seq` IntLit tyI (i^j)
        (FloatLit _ i, FloatLit _ j) -> i `seq` j `seq` FloatLit tyF (i**j)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Plus) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)        -> i `seq` j `seq` IntLit tyI (i+j)
        (StrLit _ s, StrLit _ s')       -> StrLit tyStr (s <> s') -- TODO: copy?
        (RegexLit _ rr, RegexLit _ rr') -> RegexLit tyStr (rr <> rr')
        (FloatLit _ i, FloatLit _ j)    -> i `seq` j `seq` FloatLit tyF (i+j)
        _                               -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Div) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> i `seq` j `seq` FloatLit tyF (i/j)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (UB ty' Tally) e) = do
    eI <- eNorm e
    pure $ case eI of
        StrLit _ str -> IntLit tyI (fromIntegral $ BS.length str)
        _            -> EApp ty (UB ty' Tally) eI
eNorm (EApp ty op@(UB _ TallyList) e) = do
    eI <- eNorm e
    pure $ case eI of
        (Arr _ xs) -> mkI $ fromIntegral $ V.length xs
        _          -> EApp ty op eI
eNorm (EApp ty (EApp ty' op@(BB _ Lt) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> BoolLit tyB (i < j)
        (FloatLit _ i, FloatLit _ j) -> BoolLit tyB (i < j)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Gt) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> BoolLit tyB (i > j)
        (FloatLit _ i, FloatLit _ j) -> BoolLit tyB (i > j)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Eq) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> BoolLit tyB (i == j)
        (FloatLit _ i, FloatLit _ j) -> BoolLit tyB (i == j)
        (BoolLit _ b, BoolLit _ b')  -> BoolLit tyB (b == b')
        (StrLit _ i, StrLit _ j)     -> BoolLit tyB (i == j)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Neq) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> BoolLit tyB (i /= j)
        (FloatLit _ i, FloatLit _ j) -> BoolLit tyB (i /= j)
        (StrLit _ i, StrLit _ j)     -> BoolLit tyB (i /= j)
        (BoolLit _ b, BoolLit _ b')  -> BoolLit tyB (b /= b')
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Leq) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> BoolLit tyB (i <= j)
        (FloatLit _ i, FloatLit _ j) -> BoolLit tyB (i <= j)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty (EApp ty' op@(BB _ Geq) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j)     -> BoolLit tyB (i >= j)
        (FloatLit _ i, FloatLit _ j) -> BoolLit tyB (i >= j)
        _                            -> EApp ty (EApp ty' op eI) eI'
eNorm (EApp ty0 (EApp ty1 op@(BB _ And) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (BoolLit _ b, BoolLit _ b') -> b `seq` b' `seq` BoolLit tyB (b && b')
        _                           -> EApp ty0 (EApp ty1 op eI) eI'
eNorm (EApp ty0 (EApp ty1 op@(BB _ Or) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (BoolLit _ b, BoolLit _ b') -> b `seq` b' `seq` BoolLit tyB (b || b')
        _                           -> EApp ty0 (EApp ty1 op eI) eI'
eNorm (EApp _ (EApp _ (UB _ Const) e) _) = eNorm e
eNorm (EApp ty op@(UB _ Const) e) = EApp ty op <$> eNorm e
eNorm (EApp ty op@(UB _ Dedup) e) = EApp ty op <$> eNorm e
eNorm (EApp ty op@(UB _ (At i)) e) = do
    eI <- eNorm e
    pure $ case eI of
        (Arr _ es) -> es V.! (i-1)
        _          -> EApp ty op eI
eNorm (EApp ty op@(UB _ (Select i)) e) = do
    eI <- eNorm e
    pure $ case eI of
        (Tup _ es) -> es !! (i-1)
        _          -> EApp ty op eI
eNorm (EApp ty op@(UB _ Negate) e) = do
    eI <- eNorm e
    pure $ case eI of
        (FloatLit _ f) -> mkF $ negate f
        (IntLit _ i)   -> mkI $ negate i
        _              -> EApp ty op eI
eNorm (EApp ty op@(UB _ Not) e) = do
    eI <- eNorm e
    pure $ case eI of
        (BoolLit _ b) -> BoolLit tyB (not b)
        _             -> EApp ty op eI
eNorm (EApp ty op@(UB _ IParse) e) = do
    eI <- eNorm e
    pure $ case eI of
        (StrLit _ str) -> parseAsEInt str
        _              -> EApp ty op eI
eNorm (EApp ty op@(UB _ FParse) e) = do
    eI <- eNorm e
    pure $ case eI of
        (StrLit _ str) -> parseAsF str
        _              -> EApp ty op eI
eNorm (EApp ty op@(UB (TyArr _ _ (TyB _ TyFloat)) Parse) e) = do
    eI <- eNorm e
    pure $ case eI of
        (StrLit _ str) -> parseAsF str
        _              -> EApp ty op eI
eNorm (EApp ty op@(UB (TyArr _ _ (TyB _ TyInteger)) Parse) e) = do
    eI <- eNorm e
    pure $ case eI of
        (StrLit _ str) -> parseAsEInt str
        _              -> EApp ty op eI
eNorm (EApp ty (UB _ Some) e) = do
    eI <- eNorm e
    pure $ OptionVal ty (Just eI)
-- catMaybes only works for streams atm
eNorm (EApp ty op@(UB _ CatMaybes) e) = EApp ty op <$> eNorm e
eNorm Dfn{} = desugar
eNorm ResVar{} = desugar
eNorm (Let _ (Nm _ (U i) _, b) e) = do
    b' <- eNorm b
    modify (mapBinds (IM.insert i b'))
    eNorm e
eNorm e@(Var _ (Nm _ (U i) _)) = do
    st <- gets binds
    case IM.lookup i st of
        Just e'@Var{} -> eNorm e' -- no cyclic binds
        Just e'       -> rE e' -- FIXME: set outermost type to be type of var...
        Nothing       -> pure e -- default to e in case var was bound in a lambda
eNorm (EApp ty e@Var{} e') = eNorm =<< (EApp ty <$> eNorm e <*> pure e')
eNorm (EApp _ (Lam _ (Nm _ (U i) _) e) e') = do
    e'' <- eNorm e'
    modify (mapBinds (IM.insert i e''))
    eNorm e
eNorm (EApp ty0 (EApp ty1 (EApp ty2 (TB ty3 Substr) e0) e1) e2) = do
    e0' <- eNorm e0
    e1' <- eNorm e1
    e2' <- eNorm e2
    pure $ case (e0', e1', e2') of
        (StrLit _ str, IntLit _ i, IntLit _ j) -> mkStr (substr str (fromIntegral i) (fromIntegral j))
        _                                      -> EApp ty0 (EApp ty1 (EApp ty2 (TB ty3 Substr) e0') e1') e2'
eNorm (EApp ty0 (EApp ty1 (EApp ty2 op@(TB _ Captures) e0) e1) e2) = do
    e0' <- eNorm e0
    e1' <- eNorm e1
    e2' <- eNorm e2
    pure $ case (e0', e1', e2') of
        (StrLit _ str, IntLit _ ix, RegexCompiled re) -> OptionVal (tyOpt tyStr) (mkStr <$> findCapture re str (fromIntegral ix))
        _                                             -> EApp ty0 (EApp ty1 (EApp ty2 op e0') e1') e2'
eNorm (EApp ty0 (EApp ty1 (EApp ty2 op@(TB _ AllCaptures) e0) e1) e2) = do
    e0' <- eNorm e0
    e1' <- eNorm e1
    e2' <- eNorm e2
    pure $ case (e0', e1', e2') of
        (StrLit _ str, IntLit _ ix, RegexCompiled re) -> Arr (tyV tyStr) (mkStr <$> V.fromList (captures' re str (fromIntegral ix)))
        _                                             -> EApp ty0 (EApp ty1 (EApp ty2 op e0') e1') e2'
eNorm (EApp ty0 (EApp ty1 (EApp ty2 op@(TB _ Option) e0) e1) e2) = do
    e0' <- eNorm e0
    e1' <- eNorm e1
    e2' <- eNorm e2
    case e2' of
        (OptionVal _ Nothing)  -> pure e0'
        (OptionVal _ (Just e)) -> eNorm (EApp undefined e1' e)
        _                      -> pure $ EApp ty0 (EApp ty1 (EApp ty2 op e0') e1') e2'
eNorm (EApp ty1 (EApp ty2 op@(TB _ Option) e0) e1) = do
    e0' <- eNorm e0
    e1' <- eNorm e1
    pure $ EApp ty1 (EApp ty2 op e0') e1'
eNorm (EApp ty0 (EApp ty1 op@(BB _ Match) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit _ str, RegexCompiled re) -> asTup (find' re str)
        _                                -> EApp ty0 (EApp ty1 op eI) eI'
eNorm (EApp ty0 (EApp ty1 op@(BB _ Sprintf) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit _ fmt, _) | isReady eI' -> mkStr $ sprintf fmt eI'
        _                               -> EApp ty0 (EApp ty1 op eI) eI'
eNorm (EApp ty0 (EApp ty1 op@(BB (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyVec) _))) Map) x) y) = do
    x' <- eNorm x
    y' <- eNorm y
    case y' of
        Arr _ es -> Arr undefined <$> traverse (applyUn x') es -- TODO: undefined?
        _        -> pure $ EApp ty0 (EApp ty1 op x') y'
eNorm (EApp ty0 (EApp ty1 op@(BB (TyArr _ _ (TyArr _ _ (TyApp _ (TyB _ TyOption) _))) Map) x) y) = do
    x' <- eNorm x
    y' <- eNorm y
    case y' of
        OptionVal _ e -> OptionVal undefined <$> traverse (applyUn x') e -- TODO: undefined?
        _             -> pure $ EApp ty0 (EApp ty1 op x') y'
eNorm (EApp ty0 (EApp ty1 op@(BB (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyVec) _) _)) Fold1) f) x) = do
    f' <- eNorm f
    x' <- eNorm x
    case x' of
        Arr _ es -> case V.uncons es of { Just (y, ys) -> foldE f' y ys ; Nothing -> throw EmptyFold }
        _        -> pure $ EApp ty0 (EApp ty1 op f') x'
eNorm (EApp ty0 (EApp ty1 (EApp ty2 op@(TB (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyVec) _) _))) Fold) f) x) y) = do
    f' <- eNorm f
    x' <- eNorm x
    y' <- eNorm y
    case y' of
        Arr _ es -> foldE f' x' es
        _        -> pure $ EApp ty0 (EApp ty1 (EApp ty2 op f') x') y'
eNorm (EApp ty0 (EApp ty1 (EApp ty2 op@TB{} f) x) y) = EApp ty0 <$> (EApp ty1 <$> (EApp ty2 op <$> eNorm f) <*> eNorm x) <*> eNorm y
-- we include this in case (+) has type (a->a->a) (for instance) if it is
-- normalizing a decl (which can be ambiguous/general)
eNorm (EApp ty0 (EApp ty1 op@BB{} e) e') = EApp ty0 <$> (EApp ty1 op <$> eNorm e) <*> eNorm e'
-- FIXME: monomorphize types after inlining
eNorm (EApp ty e@EApp{} e') =
    eNorm =<< (EApp ty <$> eNorm e <*> pure e')
eNorm (Arr ty es) = Arr ty <$> traverse eNorm es
eNorm (OptionVal ty e) = OptionVal ty <$> traverse eNorm e
eNorm (Cond ty p e0 e1) = do
    p' <- eNorm p
    case p' of
        BoolLit _ True  -> eNorm e0
        BoolLit _ False -> eNorm e1
        _               -> Cond ty p' <$> eNorm e0 <*> eNorm e1 -- needed to perform substitutions
eNorm e = error ("Internal error: " ++ show e)
