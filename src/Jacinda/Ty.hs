{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Ty ( TypeM
                  , Error (..)
                  , runTypeM
                  , tyE
                  -- * For debugging
                  , tyOf
                  ) where

import           Control.Exception          (Exception)
import           Control.Monad.Except       (throwError)
import           Control.Monad.State.Strict (StateT, gets, runStateT)
import           Data.Bifunctor             (second)
import           Data.Foldable              (traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST
import           Jacinda.Ty.Const
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying)
import           Prettyprinter              (Doc, Pretty (..), hardline, squotes, vsep, (<+>))

infixr 6 <#>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

data Error a = UnificationFailed a (T ()) (T ())
             | Doesn'tSatisfy (T ()) C
             | IllScoped a (Name a)

instance Pretty a => Pretty (Error a) where
    pretty (UnificationFailed l ty ty') = pretty l <+> "could not unify type" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty')
    pretty (Doesn'tSatisfy ty c)        = squotes (pretty ty) <+> "is not a member of class" <+> pretty c
    pretty (IllScoped l n)              = pretty l <+> squotes (pretty n) <+> "is not in scope."

instance Pretty a => Show (Error a) where
    show = show . pretty

instance (Typeable a, Pretty a) => Exception (Error a) where

data C = IsNum
       | IsEq
       | IsOrd
       | IsParseable
       | IsSemigroup
       -- TODO: foldable (vect.), functor &c.?
       deriving (Eq, Ord)

instance Pretty C where
    pretty IsNum       = "Num"
    pretty IsEq        = "Eq"
    pretty IsOrd       = "Ord"
    pretty IsParseable = "Parseable"
    pretty IsSemigroup = "Semigroup"

-- Idea:
-- solve, unify etc. THEN check that all constraints are satisfied?
-- (after accumulating classVar membership...)
data TyState a = TyState { maxU        :: Int
                         , kindEnv     :: IM.IntMap K
                         , classVars   :: IM.IntMap (S.Set C)
                         , varEnv      :: IM.IntMap (T K)
                         , constraints :: S.Set (a, T K, T K)
                         }

instance Pretty (TyState a) where
    pretty (TyState _ _ _ _ cs) =
        "constraints:" <#> prettyConstraints cs

prettyConstraints :: S.Set (b, T a, T a) -> Doc ann
prettyConstraints cs = vsep (prettyEq . go <$> S.toList cs) where
    go (_, x, y) = (x, y)

prettyEq :: (T a, T a) -> Doc ann
prettyEq (ty, ty') = pretty ty <+> "≡" <+> pretty ty'

maxULens :: Lens' (TyState a) Int
maxULens f s = fmap (\x -> s { maxU = x }) (f (maxU s))

classVarsLens :: Lens' (TyState a) (IM.IntMap (S.Set C))
classVarsLens f s = fmap (\x -> s { classVars = x }) (f (classVars s))

varEnvLens :: Lens' (TyState a) (IM.IntMap (T K))
varEnvLens f s = fmap (\x -> s { varEnv = x }) (f (varEnv s))

constraintsLens :: Lens' (TyState a) (S.Set (a, T K, T K))
constraintsLens f s = fmap (\x -> s { constraints = x }) (f (constraints s))

type TypeM a = StateT (TyState a) (Either (Error a))

runTypeM :: Int -> TypeM a b -> Either (Error a) (b, Int)
runTypeM i = fmap (second maxU) . flip runStateT (TyState i IM.empty IM.empty IM.empty S.empty)

type UnifyMap a = IM.IntMap (T a)

inContext :: UnifyMap a -> T a -> T a
inContext um ty'@(TyVar _ (Name _ (Unique i) _)) =
    case IM.lookup i um of
        Just ty@TyVar{} -> inContext (IM.delete i um) ty -- prevent cyclic lookups
        -- TODO: does this need a case for TyApp -> inContext?
        Just ty         -> ty
        Nothing         -> ty'
inContext _ ty'@TyB{} = ty'
inContext _ ty'@TyNamed{} = ty'
inContext um (TyApp l ty ty') = TyApp l (inContext um ty) (inContext um ty')
inContext um (TyArr l ty ty') = TyArr l (inContext um ty) (inContext um ty')
inContext um (TyTup l tys)    = TyTup l (inContext um <$> tys)

-- | Perform substitutions before handing off to 'unifyMatch'
unifyPrep :: UnifyMap a
          -> [(l, T a, T a)]
          -> TypeM l (IM.IntMap (T a))
unifyPrep _ [] = pure mempty
unifyPrep um ((l, ty, ty'):tys) =
    let ty'' = inContext um ty
        ty''' = inContext um ty'
    in unifyMatch um $ (l, ty'', ty'''):tys

unifyMatch :: UnifyMap a -> [(l, T a, T a)] -> TypeM l (IM.IntMap (T a))
unifyMatch _ [] = pure mempty
unifyMatch um ((_, TyB _ b, TyB _ b'):tys) | b == b' = unifyPrep um tys
unifyMatch um ((_, TyNamed _ n0, TyNamed _ n1):tys) | n0 == n1 = unifyPrep um tys
unifyMatch um ((_, ty@TyB{}, TyVar  _ (Name _ (Unique k) _)):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((_, TyVar _ (Name _ (Unique k) _), ty@(TyB{})):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((_, ty@TyArr{}, TyVar  _ (Name _ (Unique k) _)):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((_, TyVar _ (Name _ (Unique k) _), ty@(TyArr{})):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((_, ty@TyApp{}, TyVar  _ (Name _ (Unique k) _)):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((_, TyVar _ (Name _ (Unique k) _), ty@(TyTup{})):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((_, ty@TyTup{}, TyVar  _ (Name _ (Unique k) _)):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((_, TyVar _ (Name _ (Unique k) _), ty@(TyApp{})):tys) = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch um ((l, TyApp _ ty ty', TyApp _ ty'' ty'''):tys) = unifyPrep um ((l, ty, ty'') : (l, ty', ty''') : tys)
unifyMatch um ((l, TyArr _ ty ty', TyArr _ ty'' ty'''):tys) = unifyPrep um ((l, ty, ty'') : (l, ty', ty''') : tys)
unifyMatch um ((_, TyVar _ n@(Name _ (Unique k) _), ty@(TyVar _ n')):tys)
    | n == n' = unifyPrep um tys -- a type variable is always equal to itself, don't bother inserting this!
    | otherwise = IM.insert k ty <$> unifyPrep (IM.insert k ty um) tys
unifyMatch _ ((l, ty, ty'):_) = throwError (UnificationFailed l (void ty) (void ty'))

unify :: [(l, T a, T a)] -> TypeM l (IM.IntMap (T a))
unify = unifyPrep IM.empty

unifyM :: S.Set (l, T a, T a) -> TypeM l (IM.IntMap (T a))
unifyM s = unify (S.toList s)

substInt :: IM.IntMap (T a) -> Int -> Maybe (T a)
substInt tys k =
    case IM.lookup k tys of
        Just ty'@TyVar{}       -> Just $ substConstraints (IM.delete k tys) ty' -- TODO: this is to prevent cyclic lookups: is it right?
        Just (TyApp l ty0 ty1) -> Just $ let tys' = IM.delete k tys in TyApp l (substConstraints tys' ty0) (substConstraints tys' ty1)
        Just (TyArr l ty0 ty1) -> Just $ let tys' = IM.delete k tys in TyArr l (substConstraints tys' ty0) (substConstraints tys' ty1)
        Just (TyTup l tysϵ)    -> Just $ let tys' = IM.delete k tys in TyTup l (substConstraints tys' <$> tysϵ)
        Just ty'               -> Just ty'
        Nothing                -> Nothing

substConstraints :: IM.IntMap (T a) -> T a -> T a
substConstraints _ ty@TyB{}                             = ty
substConstraints tys ty@(TyVar _ (Name _ (Unique k) _)) = fromMaybe ty (substInt tys k)
substConstraints tys (TyTup l tysϵ)                     = TyTup l (substConstraints tys <$> tysϵ)
substConstraints tys (TyApp l ty ty')                   =
    TyApp l (substConstraints tys ty) (substConstraints tys ty')
substConstraints tys (TyArr l ty ty')                   =
    TyArr l (substConstraints tys ty) (substConstraints tys ty')

-- all names are of kind 'Star'
dummyName :: T.Text -> TypeM a (Name K)
dummyName n = do
    st <- gets maxU
    Name n (Unique $ st+1) Star
        <$ modifying maxULens (+1)

addC :: Name a -> C -> IM.IntMap (S.Set C) -> IM.IntMap (S.Set C)
addC (Name _ (Unique i) _) c = IM.alter (Just . go) i where
    go Nothing   = S.singleton c
    go (Just cs) = S.insert c cs

-- | arguments assumed to have kind 'Star'
tyArr :: T K -> T K -> T K
tyArr = TyArr Star

var :: Name K -> T K
var = TyVar Star

-- assumes they have been renamed...
pushConstraint :: Ord a => a -> T K -> T K -> TypeM a ()
pushConstraint l ty ty' =
    modifying constraintsLens (S.insert (l, ty, ty'))

-- TODO: this will need some class context if we permit custom types (Optional)
checkType :: T b -> C -> TypeM a ()
checkType (TyB _ TyStr) IsSemigroup     = pure ()
checkType (TyB _ TyInteger) IsSemigroup = pure ()
checkType (TyB _ TyInteger) IsNum       = pure ()
checkType (TyB _ TyInteger) IsOrd       = pure ()
checkType (TyB _ TyInteger) IsEq        = pure ()
checkType (TyB _ TyInteger) IsParseable = pure ()
checkType (TyB _ TyFloat) IsParseable   = pure ()
checkType (TyB _ TyFloat) IsSemigroup   = pure ()
checkType (TyB _ TyFloat) IsNum         = pure ()
checkType (TyB _ TyFloat) IsOrd         = pure ()
checkType (TyB _ TyFloat) IsEq          = pure ()
checkType (TyB _ TyBool) IsEq           = pure ()
checkType (TyB _ TyStr) IsEq            = pure ()
checkType (TyTup _ tys) IsEq            = traverse_ (`checkType` IsEq) tys
checkType (TyTup _ tys) IsOrd           = traverse_ (`checkType` IsOrd) tys
checkType ty@TyTup{} c@IsNum            = throwError $ Doesn'tSatisfy (void ty) c
checkType ty@(TyB _ TyStr) c@IsNum      = throwError $ Doesn'tSatisfy (void ty) c
checkType ty@(TyB _ TyBool) c@IsNum     = throwError $ Doesn'tSatisfy (void ty) c
checkType ty@TyArr{} c                  = throwError $ Doesn'tSatisfy (void ty) c
-- FIXME: when we encounter a type variable at this stage I think it's ok?
-- TODO: maybe streams could have num + eq instances...

checkClass :: IM.IntMap (T K) -- ^ Unification result
           -> Int
           -> S.Set C
           -> TypeM a ()
checkClass tys i cs =
    case substInt tys i of
        Just ty -> traverse_ (checkType ty) (S.toList cs)
        Nothing -> error "idk what to do in this case." -- TODO:
        -- FIXME: to allow generic user functions,

lookupVar :: Name a -> TypeM a (T K)
lookupVar n@(Name _ (Unique i) l) = do
    st <- gets varEnv
    case IM.lookup i st of
        Just ty -> pure ty
        Nothing -> throwError $ IllScoped l n

tyOf :: Ord a => E a -> TypeM a (T K)
tyOf = fmap eLoc . tyE

-- FIXME kind check
tyE :: Ord a => E a -> TypeM a (E (T K))
tyE e = do
    e' <- tyE0 e
    backNames <- unifyM =<< gets constraints
    toCheck <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass backNames)) toCheck
    pure (fmap (substConstraints backNames) e')

tyNumOp :: TypeM a (T K)
tyNumOp = do
    m <- dummyName "m"
    modifying classVarsLens (addC m IsNum)
    let m' = var m
    pure $ tyArr m' (tyArr m' m')

tySemiOp :: TypeM a (T K)
tySemiOp = do
    m <- dummyName "m"
    modifying classVarsLens (addC m IsSemigroup)
    let m' = var m
    pure $ tyArr m' (tyArr m' m')

tyOrd :: TypeM a (T K)
tyOrd = do
    a <- dummyName "a"
    modifying classVarsLens (addC a IsOrd)
    let a' = var a
    pure $ tyArr a' (tyArr a' tyBool)

tyEq :: TypeM a (T K)
tyEq = do
    a <- dummyName "a"
    modifying classVarsLens (addC a IsEq)
    let a' = var a
    pure $ tyArr a' (tyArr a' tyBool)

-- min/max
tyM :: TypeM a (T K)
tyM = do
    a <- dummyName "a"
    modifying classVarsLens (addC a IsOrd)
    let a' = var a
    pure $ tyArr a' (tyArr a' a')

desugar :: a
desugar = error "Should have been de-sugared in an earlier stage!"

tyE0 :: Ord a => E a -> TypeM a (E (T K))
tyE0 (BoolLit _ b)      = pure $ BoolLit tyBool b
tyE0 (IntLit _ i)       = pure $ IntLit tyI i
tyE0 (FloatLit _ f)     = pure $ FloatLit tyF f
tyE0 (StrLit _ str)     = pure $ StrLit tyStr str
tyE0 (RegexLit _ rr)    = pure $ RegexLit tyStr rr
tyE0 (Column _ i)       = pure $ Column (tyStream tyStr) i
tyE0 (IParseCol _ i)    = pure $ IParseCol (tyStream tyI) i
tyE0 (IParseField _ i)  = pure $ IParseField tyI i
tyE0 (FParseCol _ i)    = pure $ FParseCol (tyStream tyF) i
tyE0 (FParseField _ i)  = pure $ FParseField tyF i
tyE0 (Field _ i)        = pure $ Field tyStr i
tyE0 AllField{}         = pure $ AllField tyStr
tyE0 AllColumn{}        = pure $ AllColumn (tyStream tyStr)
tyE0 Ix{}               = pure $ Ix tyI
tyE0 (BBuiltin _ Plus)  = BBuiltin <$> tySemiOp <*> pure Plus
tyE0 (BBuiltin _ Minus) = BBuiltin <$> tyNumOp <*> pure Minus
tyE0 (BBuiltin _ Times) = BBuiltin <$> tyNumOp <*> pure Times
tyE0 (BBuiltin _ Gt)    = BBuiltin <$> tyOrd <*> pure Gt
tyE0 (BBuiltin _ Lt)    = BBuiltin <$> tyOrd <*> pure Lt
tyE0 (BBuiltin _ Geq)   = BBuiltin <$> tyOrd <*> pure Geq
tyE0 (BBuiltin _ Leq)   = BBuiltin <$> tyOrd <*> pure Leq
tyE0 (BBuiltin _ Eq)    = BBuiltin <$> tyEq <*> pure Eq
tyE0 (BBuiltin _ Neq)   = BBuiltin <$> tyEq <*> pure Neq
tyE0 (BBuiltin _ Min)   = BBuiltin <$> tyM <*> pure Min
tyE0 (BBuiltin _ Max)   = BBuiltin <$> tyM <*> pure Max
tyE0 (BBuiltin _ Matches) = pure $ BBuiltin (tyArr tyStr (tyArr tyStr tyBool)) Matches
tyE0 (BBuiltin _ NotMatches) = pure $ BBuiltin (tyArr tyStr (tyArr tyStr tyBool)) NotMatches
tyE0 (UBuiltin _ Tally) = pure $ UBuiltin (tyArr tyStr tyI) Tally
tyE0 (BBuiltin _ Div)   = pure $ BBuiltin (tyArr tyF (tyArr tyF tyF)) Div
tyE0 (UBuiltin _ Not)   = pure $ UBuiltin (tyArr tyBool tyBool) Not
tyE0 (BBuiltin _ And)   = pure $ BBuiltin (tyArr tyBool (tyArr tyBool tyBool)) And
tyE0 (BBuiltin _ Or)    = pure $ BBuiltin (tyArr tyBool (tyArr tyBool tyBool)) Or
tyE0 (UBuiltin _ Const) = do
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
        fTy = tyArr a' (tyArr b' a')
    pure $ UBuiltin fTy Const
tyE0 (BBuiltin _ Filter) = do
    a <- dummyName "a"
    let a' = var a
        fTy = tyArr (tyArr a' tyBool) (tyArr (tyStream a') (tyStream a'))
    pure $ BBuiltin fTy Filter
tyE0 (BBuiltin _ Map) = do
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
        fTy = tyArr (tyArr a' b') (tyArr (tyStream a') (tyStream b'))
    pure $ BBuiltin fTy Map
-- (b -> a -> b) -> b -> Stream a -> b
tyE0 (TBuiltin _ Fold) = do
    b <- dummyName "b"
    a <- dummyName "a"
    let b' = var b
        a' = var a
        fTy = tyArr (tyArr b' (tyArr a' b')) (tyArr b' (tyArr (tyStream a') b'))
    pure $ TBuiltin fTy Fold
-- (a -> a -> a) -> Stream a -> Stream a
tyE0 (BBuiltin _ Prior) = do
    a <- dummyName "a"
    let a' = var a
        fTy = tyArr (tyArr a' (tyArr a' a')) (tyArr (tyStream a') (tyStream a'))
    pure $ BBuiltin fTy Prior
-- (a -> b -> c) -> Stream a -> Stream b -> Stream c
tyE0 (TBuiltin _ ZipW) = do
    a <- dummyName "a"
    b <- dummyName "b"
    c <- dummyName "c"
    let a' = var a
        b' = var b
        c' = var c
        fTy = tyArr (tyArr a' (tyArr b' c')) (tyArr (tyStream a') (tyArr (tyStream b') (tyStream c')))
    pure $ TBuiltin fTy ZipW
-- (b -> a -> b) -> b -> Stream a -> Stream b
tyE0 (TBuiltin _ Scan) = do
    b <- dummyName "b"
    a <- dummyName "a"
    let b' = var b
        a' = var a
        fTy = tyArr (tyArr b' (tyArr a' b')) (tyArr b' (tyArr (tyStream a') (tyStream b')))
    pure $ TBuiltin fTy Scan
-- (a -> b -> c) -> Stream a -> Stream b -> Stream c
tyE0 (Guarded l e streamE) = do
    streamE' <- tyE0 streamE
    e' <- tyE0 e
    pushConstraint l tyBool (eLoc e')
    pure $ Guarded (tyStream (eLoc streamE')) e' streamE'
tyE0 (EApp _ e0 e1) = do
    e0' <- tyE0 e0
    e1' <- tyE0 e1
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
        fTy = tyArr a' b'
    pushConstraint (eLoc e0) fTy (eLoc e0')
    pushConstraint (eLoc e1) a' (eLoc e1')
    pure $ EApp b' e0' e1'
tyE0 (Lam _ n@(Name _ (Unique i) _) e) = do
    a <- dummyName "a"
    let a' = var a
    modifying varEnvLens (IM.insert i a')
    e' <- tyE0 e
    pure $ Lam (tyArr a' (eLoc e')) (n $> a') e'
tyE0 (Let _ (n@(Name _ (Unique i) _), eϵ) e) = do
    eϵ' <- tyE0 eϵ
    let bTy = eLoc eϵ'
    modifying varEnvLens (IM.insert i bTy)
    e' <- tyE0 e
    pure $ Let (eLoc e') (n $> bTy, eϵ') e'
tyE0 (Tup _ es) = do
    es' <- traverse tyE0 es
    pure $ Tup (TyTup Star (eLoc <$> es')) es'
tyE0 (Var _ n) = do
    ty <- lookupVar n
    pure (Var ty (n $> ty))
tyE0 Dfn{} = desugar
tyE0 (ResVar _ X) = desugar
tyE0 (ResVar _ Y) = desugar
tyE0 RegexCompiled{} = error "Regex should not be compiled at this stage."
