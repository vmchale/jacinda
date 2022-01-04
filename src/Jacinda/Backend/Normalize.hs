-- TODO: test this module?
module Jacinda.Backend.Normalize ( compileR
                                 , eClosed
                                 , desugar
                                 ) where

import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Control.Recursion          (cata, embed)
import qualified Data.ByteString            as BS
import qualified Data.IntMap                as IM
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST
import           Jacinda.Regex
import           Jacinda.Rename
import           Jacinda.Ty.Const

-- fill in regex with compiled.
compileR :: E (T K)
         -> E (T K)
compileR = cata a where -- TODO: combine with eNorm pass?
    a (RegexLitF _ rr) = RegexCompiled (compileDefault rr)
    a x                = embed x

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

eClosed :: Int
        -> E (T K)
        -> E (T K)
eClosed i = flip evalState (LetCtx IM.empty (Renames i IM.empty)) . eNorm

eNorm :: E (T K)
      -> EvalM (E (T K))
eNorm e@Field{}       = pure e
eNorm e@IParseField{} = pure e
eNorm e@FParseField{} = pure e
eNorm e@IntLit{}      = pure e
eNorm e@FloatLit{}    = pure e
eNorm e@BoolLit{}     = pure e
eNorm e@StrLit{}      = pure e
eNorm e@RegexLit{}    = pure e
eNorm e@RegexCompiled{} = pure e
eNorm e@UBuiltin{}    = pure e
eNorm e@Column{}      = pure e
eNorm e@AllColumn{}   = pure e
eNorm e@IParseCol{}   = pure e
eNorm e@FParseCol{}   = pure e
eNorm e@AllField{}    = pure e
eNorm (Guarded ty pe e) = Guarded ty <$> eNorm pe <*> eNorm e
eNorm (Lam ty n e)    = Lam ty n <$> eNorm e
eNorm e@BBuiltin{}    = pure e
eNorm e@TBuiltin{}    = pure e
eNorm (Tup tys es)    = Tup tys <$> traverse eNorm es
eNorm e@Ix{}          = pure e
eNorm (EApp ty op@BBuiltin{} e) = EApp ty op <$> eNorm e
eNorm e0@(EApp _ (EApp _ (BBuiltin _ Matches) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (isMatch' re str)
        (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (isMatch' re str)
        _                                -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin _ NotMatches) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (not $ isMatch' re str)
        (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (not $ isMatch' re str)
        _                                -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Plus) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (i+j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Plus) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit _ s, StrLit _ s')       -> StrLit tyStr (s <> s')
        (RegexLit _ rr, RegexLit _ rr') -> RegexLit tyStr (rr <> rr')
        -- TODO: str + regex? eh
        _                               -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Max) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (max i j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Min) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (min i j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Max) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ x, FloatLit _ y) -> FloatLit tyF (max x y)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Min) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ x, FloatLit _ y) -> FloatLit tyF (min x y)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Minus) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (i-j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Times) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> IntLit tyI (i*j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Plus) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i+j)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Minus) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i-j)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Times) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i*j)
        _                            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Div) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i/j)
        _                            -> e0
eNorm e0@(EApp _ (UBuiltin _ Tally) e) = do
    eI <- eNorm e
    pure $ case eI of
        StrLit _ str -> IntLit tyI (fromIntegral $ BS.length str)
        _            -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Lt) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i < j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Gt) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i > j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Eq) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i == j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Eq) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit _ i, StrLit _ j) -> BoolLit tyBool (i == j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Neq) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i /= j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Neq) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (StrLit _ i, StrLit _ j) -> BoolLit tyBool (i /= j)
        _                        -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin _ And) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (BoolLit _ b, BoolLit _ b') -> BoolLit tyBool (b && b')
        _                           -> e0
eNorm e0@(EApp _ (EApp _ (BBuiltin _ Or) e) e') = do
    eI <- eNorm e
    eI' <- eNorm e'
    pure $ case (eI, eI') of
        (BoolLit _ b, BoolLit _ b') -> BoolLit tyBool (b || b')
        _                           -> e0
eNorm (EApp _ (EApp _ (UBuiltin _ Const) e) _) = pure e
eNorm e@(EApp _ (UBuiltin _ Const) _) = pure e
eNorm Dfn{} = desugar
eNorm ResVar{} = desugar
eNorm (Let _ (Name _ (Unique i) _, b) e) = do
    b' <- eNorm b
    modify (mapBinds (IM.insert i b'))
    eNorm e
eNorm e@(Var _ (Name _ (Unique i) _)) = do
    st <- gets binds
    case IM.lookup i st of
        Just e'@Var{} -> eNorm e' -- no cyclic binds!!
        Just e'       -> renameE e'
        Nothing       -> pure e -- default to e in case var was bound in a lambda
eNorm (EApp ty e@Var{} e') = EApp ty <$> eNorm e <*> eNorm e'
eNorm (EApp _ (Lam _ (Name _ (Unique i) _) e) e') = do
    e'' <- eNorm e'
    modify (mapBinds (IM.insert i e''))
    eNorm e
eNorm (EApp ty0 (EApp ty1 (EApp ty2 op@TBuiltin{} f) x) y) = EApp ty0 <$> (EApp ty1 <$> (EApp ty2 op <$> eNorm f) <*> eNorm x) <*> eNorm y
-- FIXME: this will almost surely run into trouble; if the above pattern matches
-- are not complete it will bottom!
eNorm (EApp ty0 (EApp ty1 op@(BBuiltin _ Prior) x) y) = EApp ty0 <$> (EApp ty1 op <$> eNorm x) <*> eNorm y
eNorm (EApp ty0 (EApp ty1 op@(BBuiltin _ Map) x) y) = EApp ty0 <$> (EApp ty1 op <$> eNorm x) <*> eNorm y
eNorm (EApp ty0 (EApp ty1 op@(BBuiltin _ Filter) x) y) = EApp ty0 <$> (EApp ty1 op <$> eNorm x) <*> eNorm y
eNorm (EApp ty e@EApp{} e') =
    eNorm =<< (EApp ty <$> eNorm e <*> pure e') -- don't normalize e' yet; hopefully it'll get done.
