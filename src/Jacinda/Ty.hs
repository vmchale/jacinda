{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Ty ( TypeM
                  , Error (..)
                  , runTypeM
                  , tyProgram
                  -- * For debugging
                  , tyOf
                  ) where

import           Control.Exception          (Exception)
import           Control.Monad              (forM)
import           Control.Monad.Except       (throwError)
import           Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import           Data.Bifunctor             (first, second)
import           Data.Foldable              (traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import           Data.Maybe                 (fromMaybe)
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import qualified Data.Vector                as V
import qualified Data.Vector.Ext            as V
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST
import           Jacinda.Ty.Const
import           Prettyprinter              (Doc, Pretty (..), hardline, squotes, vsep, (<+>))

infixr 6 <#>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

data Error a = UnificationFailed a (T ()) (T ())
             | Doesn'tSatisfy a (T ()) C
             | IllScoped a (Name a)
             | Ambiguous (E ())

instance Pretty a => Pretty (Error a) where
    pretty (UnificationFailed l ty ty') = pretty l <+> "could not unify type" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty')
    pretty (Doesn'tSatisfy l ty c)      = pretty l <+> squotes (pretty ty) <+> "is not a member of class" <+> pretty c
    pretty (IllScoped l n)              = pretty l <+> squotes (pretty n) <+> "is not in scope."
    pretty (Ambiguous e)                = "type of" <+> squotes (pretty e) <+> "is ambiguous"

instance Pretty a => Show (Error a) where
    show = show . pretty

instance (Typeable a, Pretty a) => Exception (Error a) where

-- solve, unify etc. THEN check that all constraints are satisfied?
-- (after accumulating classVar membership...)
data TyState a = TyState { maxU        :: Int
                         , kindEnv     :: IM.IntMap K
                         , classVars   :: IM.IntMap (S.Set (C, a))
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

mapMaxU :: (Int -> Int) -> TyState a -> TyState a
mapMaxU f (TyState u k c v cs) = TyState (f u) k c v cs

mapClassVars :: (IM.IntMap (S.Set (C, a)) -> IM.IntMap (S.Set (C, a))) -> TyState a -> TyState a
mapClassVars f (TyState u k cvs v cs) = TyState u k (f cvs) v cs

addVarEnv :: Int -> T K -> TyState a -> TyState a
addVarEnv i ty (TyState u k cvs v cs) = TyState u k cvs (IM.insert i ty v) cs

addConstraint :: Ord a => (a, T K, T K) -> TyState a -> TyState a
addConstraint c (TyState u k cvs v cs) = TyState u k cvs v (S.insert c cs)

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
unifyMatch um ((l, ty@(TyTup _ tys), ty'@(TyTup _ tys')):tyss)
    | length tys == length tys' = unifyPrep um (zip3 (repeat l) tys tys' ++ tyss)
    | otherwise = throwError (UnificationFailed l (void ty) (void ty'))
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
substConstraints _ ty@TyNamed{}                         = ty
substConstraints tys ty@(TyVar _ (Name _ (Unique k) _)) = fromMaybe ty (substInt tys k)
substConstraints tys (TyTup l tysϵ)                     = TyTup l (substConstraints tys <$> tysϵ)
substConstraints tys (TyApp l ty ty')                   =
    TyApp l (substConstraints tys ty) (substConstraints tys ty')
substConstraints tys (TyArr l ty ty')                   =
    TyArr l (substConstraints tys ty) (substConstraints tys ty')

freshName :: T.Text -> K -> TypeM a (Name K)
freshName n k = do
    st <- gets maxU
    Name n (Unique $ st+1) k
        <$ modify (mapMaxU (+1))

higherOrder :: T.Text -> TypeM a (Name K)
higherOrder t = freshName t (KArr Star Star)

-- of kind 'Star'
dummyName :: T.Text -> TypeM a (Name K)
dummyName n = freshName n Star

addC :: Ord a => Name b -> (C, a) -> IM.IntMap (S.Set (C, a)) -> IM.IntMap (S.Set (C, a))
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
    modify (addConstraint (l, ty, ty'))

-- TODO: this will need some class context if we permit custom types (Optional)
checkType :: Ord a => T K -> (C, a) -> TypeM a ()
checkType TyVar{} _                            = pure () -- TODO: I think this is right
checkType (TyB _ TyStr) (IsSemigroup, _)       = pure ()
checkType (TyB _ TyInteger) (IsSemigroup, _)   = pure ()
checkType (TyB _ TyInteger) (IsNum, _)         = pure ()
checkType (TyB _ TyInteger) (IsOrd, _)         = pure ()
checkType (TyB _ TyInteger) (IsEq, _)          = pure ()
checkType (TyB _ TyInteger) (IsParseable, _)   = pure ()
checkType (TyB _ TyFloat) (IsParseable, _)     = pure ()
checkType ty (IsParseable, l)                  = throwError $ Doesn'tSatisfy l (void ty) IsParseable
checkType (TyB _ TyFloat) (IsSemigroup, _)     = pure ()
checkType (TyB _ TyFloat) (IsNum, _)           = pure ()
checkType (TyB _ TyFloat) (IsOrd, _)           = pure ()
checkType (TyB _ TyFloat) (IsEq, _)            = pure ()
checkType (TyB _ TyBool) (IsEq, _)             = pure ()
checkType (TyB _ TyStr) (IsEq, _)              = pure ()
checkType (TyTup _ tys) (IsEq, l)              = traverse_ (`checkType` (IsEq, l)) tys
checkType (TyTup _ tys) (IsOrd, l)             = traverse_ (`checkType` (IsOrd, l)) tys
checkType (TyApp _ (TyB _ TyVec) ty) (IsEq, l) = checkType ty (IsEq, l)
checkType ty@TyTup{} (c@IsNum, l)              = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty@(TyB _ TyStr) (c@IsNum, l)        = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty@(TyB _ TyBool) (c@IsNum, l)       = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty@TyArr{} (c, l)                    = throwError $ Doesn'tSatisfy l (void ty) c
checkType (TyB _ TyVec) (Functor, _)           = pure ()
checkType (TyB _ TyStream) (Functor, _)        = pure ()
checkType (TyB _ TyOption) (Functor, _)        = pure ()
checkType (TyB _ TyStream) (Witherable, _)     = pure ()
checkType ty (c@Witherable, l)                 = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty (c@Functor, l)                    = throwError $ Doesn'tSatisfy l (void ty) c
checkType (TyB _ TyVec) (Foldable, _)          = pure ()
checkType (TyB _ TyStream) (Foldable, _)       = pure ()
checkType ty (c@Foldable, l)                   = throwError $ Doesn'tSatisfy l (void ty) c
checkType (TyB _ TyStr) (IsPrintf, _)          = pure ()
checkType (TyB _ TyFloat) (IsPrintf, _)        = pure ()
checkType (TyB _ TyInteger) (IsPrintf, _)      = pure ()
checkType (TyB _ TyBool) (IsPrintf, _)         = pure ()
checkType (TyTup _ tys) (IsPrintf, l)          = traverse_ (`checkType` (IsPrintf, l)) tys
checkType ty (c@IsPrintf, l)                   = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty@(TyTup _ tys) (c@(HasField i ty'), l) | length tys >= i = pushConstraint l ty' (tys !! (i-1))
                                                   | otherwise = throwError $ Doesn'tSatisfy l (void ty) c
checkType ty (c@HasField{}, l)                 = throwError $ Doesn'tSatisfy l (void ty) c

substC :: IM.IntMap (T K) -- ^ Unification result
       -> C
       -> C
substC um (HasField i ty) = HasField i (substConstraints um ty)
substC _ c                = c

checkClass :: Ord a
           => IM.IntMap (T K) -- ^ Unification result
           -> Int
           -> S.Set (C, a)
           -> TypeM a ()
checkClass tys i cs =
    case substInt tys i of
        Just ty -> traverse_ (checkType ty) (first (substC tys) <$> S.toList cs)
        Nothing -> pure () -- FIXME: do we need to check var is well-kinded for constraint?

lookupVar :: Name a -> TypeM a (T K)
lookupVar n@(Name _ (Unique i) l) = do
    st <- gets varEnv
    case IM.lookup i st of
        Just ty -> pure ty
        Nothing -> throwError $ IllScoped l n

tyOf :: Ord a => E a -> TypeM a (T K)
tyOf = fmap eLoc . tyE

tyD0 :: Ord a => D a -> TypeM a (D (T K))
tyD0 (SetFS bs) = pure $ SetFS bs
tyD0 (FunDecl n@(Name _ (Unique i) _) [] e) = do
    e' <- tyE0 e
    let ty = eLoc e'
    modify (addVarEnv i ty)
    pure $ FunDecl (n $> ty) [] e'
tyD0 FunDecl{} = error "Internal error. Should have been desugared by now."

isAmbiguous :: T K -> Bool
isAmbiguous TyVar{}          = True
isAmbiguous (TyArr _ ty ty') = isAmbiguous ty || isAmbiguous ty'
isAmbiguous (TyApp _ ty ty') = isAmbiguous ty || isAmbiguous ty'
isAmbiguous (TyTup _ tys)    = any isAmbiguous tys
isAmbiguous TyNamed{}        = False
isAmbiguous TyB{}            = False

checkAmb :: E (T K) -> TypeM a ()
checkAmb e@(BBuiltin ty _) | isAmbiguous ty = throwError $ Ambiguous (void e)
checkAmb TBuiltin{} = pure () -- don't fail on ternary builtins, we don't need it anyway... better error messages
checkAmb e@(UBuiltin ty _) | isAmbiguous ty = throwError $ Ambiguous (void e)
checkAmb (Implicit _ e') = checkAmb e'
checkAmb (Guarded _ p e') = checkAmb p *> checkAmb e'
checkAmb (EApp _ e' e'') = checkAmb e' *> checkAmb e'' -- more precise errors, don't fail yet! (if they aren't ambiguous, it shouldn't be
checkAmb (Tup _ es) = traverse_ checkAmb es
checkAmb e@(Arr ty _) | isAmbiguous ty = throwError $ Ambiguous (void e)
checkAmb e@(Var ty _) | isAmbiguous ty = throwError $ Ambiguous (void e)
checkAmb (Let _ bs e) = traverse_ checkAmb [e, snd bs]
checkAmb (Lam _ _ e) = checkAmb e -- I think
checkAmb _ = pure ()

tyProgram :: Ord a => Program a -> TypeM a (Program (T K))
tyProgram (Program ds e) = do
    ds' <- traverse tyD0 ds
    e' <- tyE0 e
    backNames <- unifyM =<< gets constraints
    toCheck <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass backNames)) toCheck
    backNames' <- unifyM =<< gets constraints
    -- FIXME: not sure if termination/whatever is guaranteed, need 2 think..
    let res = fmap (substConstraints backNames') (Program ds' e')
    checkAmb (expr res) $> res

-- FIXME kind check
tyE :: Ord a => E a -> TypeM a (E (T K))
tyE e = do
    e' <- tyE0 e
    backNames <- unifyM =<< gets constraints
    toCheck <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass backNames)) toCheck
    pure (fmap (substConstraints backNames) e')

tyNumOp :: Ord a => a -> TypeM a (T K)
tyNumOp l = do
    m <- dummyName "m"
    modify (mapClassVars (addC m (IsNum, l)))
    let m' = var m
    pure $ tyArr m' (tyArr m' m')

tySemiOp :: Ord a => a -> TypeM a (T K)
tySemiOp l = do
    m <- dummyName "m"
    modify (mapClassVars (addC m (IsSemigroup, l)))
    let m' = var m
    pure $ tyArr m' (tyArr m' m')

tyOrd :: Ord a => a -> TypeM a (T K)
tyOrd l = do
    a <- dummyName "a"
    modify (mapClassVars (addC a (IsOrd, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' tyBool)

tyEq :: Ord a => a -> TypeM a (T K)
tyEq l = do
    a <- dummyName "a"
    modify (mapClassVars (addC a (IsEq, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' tyBool)

-- min/max
tyM :: Ord a => a -> TypeM a (T K)
tyM l = do
    a <- dummyName "a"
    modify (mapClassVars (addC a (IsOrd, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' a')

desugar :: a
desugar = error "Should have been de-sugared in an earlier stage!"

hkt :: T K -> T K -> T K
hkt = TyApp Star

tyVec :: T K
tyVec = TyB (KArr Star Star) TyVec

mkVec :: T K -> T K
mkVec = hkt tyVec

tyOpt :: T K -> T K
tyOpt = hkt (TyB (KArr Star Star) TyOption)

tyE0 :: Ord a => E a -> TypeM a (E (T K))
tyE0 (BoolLit _ b)           = pure $ BoolLit tyBool b
tyE0 (IntLit _ i)            = pure $ IntLit tyI i
tyE0 (FloatLit _ f)          = pure $ FloatLit tyF f
tyE0 (StrLit _ str)          = pure $ StrLit tyStr str
tyE0 (RegexLit _ rr)         = pure $ RegexLit tyStr rr
tyE0 (Column _ i)            = pure $ Column (tyStream tyStr) i
tyE0 (IParseCol _ i)         = pure $ IParseCol (tyStream tyI) i
tyE0 (FParseCol _ i)         = pure $ FParseCol (tyStream tyF) i
tyE0 (Field _ i)             = pure $ Field tyStr i
tyE0 AllField{}              = pure $ AllField tyStr
tyE0 AllColumn{}             = pure $ AllColumn (tyStream tyStr)
tyE0 (NBuiltin _ Ix)         = pure $ NBuiltin tyI Ix
tyE0 (NBuiltin _ Fp)         = pure $ NBuiltin tyStr Fp
tyE0 (NBuiltin _ Nf)         = pure $ NBuiltin tyI Nf
tyE0 (BBuiltin l Plus)       = BBuiltin <$> tySemiOp l <*> pure Plus
tyE0 (BBuiltin l Minus)      = BBuiltin <$> tyNumOp l <*> pure Minus
tyE0 (BBuiltin l Times)      = BBuiltin <$> tyNumOp l <*> pure Times
tyE0 (BBuiltin l Gt)         = BBuiltin <$> tyOrd l <*> pure Gt
tyE0 (BBuiltin l Lt)         = BBuiltin <$> tyOrd l <*> pure Lt
tyE0 (BBuiltin l Geq)        = BBuiltin <$> tyOrd l <*> pure Geq
tyE0 (BBuiltin l Leq)        = BBuiltin <$> tyOrd l <*> pure Leq
tyE0 (BBuiltin l Eq)         = BBuiltin <$> tyEq l <*> pure Eq
tyE0 (BBuiltin l Neq)        = BBuiltin <$> tyEq l <*> pure Neq
tyE0 (BBuiltin l Min)        = BBuiltin <$> tyM l <*> pure Min
tyE0 (BBuiltin l Max)        = BBuiltin <$> tyM l <*> pure Max
tyE0 (BBuiltin _ Split)      = pure $ BBuiltin (tyArr tyStr (tyArr tyStr (mkVec tyStr))) Split
tyE0 (BBuiltin _ Splitc)     = pure $ BBuiltin (tyArr tyStr (tyArr tyStr (mkVec tyStr))) Splitc
tyE0 (BBuiltin _ Matches)    = pure $ BBuiltin (tyArr tyStr (tyArr tyStr tyBool)) Matches
tyE0 (BBuiltin _ NotMatches) = pure $ BBuiltin (tyArr tyStr (tyArr tyStr tyBool)) NotMatches
tyE0 (UBuiltin _ Tally)      = pure $ UBuiltin (tyArr tyStr tyI) Tally
tyE0 (BBuiltin _ Div)        = pure $ BBuiltin (tyArr tyF (tyArr tyF tyF)) Div
tyE0 (UBuiltin _ Not)        = pure $ UBuiltin (tyArr tyBool tyBool) Not
tyE0 (BBuiltin _ And)        = pure $ BBuiltin (tyArr tyBool (tyArr tyBool tyBool)) And
tyE0 (BBuiltin _ Or)         = pure $ BBuiltin (tyArr tyBool (tyArr tyBool tyBool)) Or
tyE0 (BBuiltin _ Match)      = pure $ BBuiltin (tyArr tyStr (tyArr tyStr (tyOpt $ TyTup Star [tyI, tyI]))) Match
tyE0 (TBuiltin _ Substr)     = pure $ TBuiltin (tyArr tyStr (tyArr tyI (tyArr tyI tyStr))) Substr
tyE0 (UBuiltin _ IParse)     = pure $ UBuiltin (tyArr tyStr tyI) IParse
tyE0 (UBuiltin _ FParse)     = pure $ UBuiltin (tyArr tyStr tyF) FParse
tyE0 (UBuiltin _ Floor)      = pure $ UBuiltin (tyArr tyF tyI) Floor
tyE0 (UBuiltin _ Ceiling)    = pure $ UBuiltin (tyArr tyF tyI) Ceiling
tyE0 (UBuiltin _ Some) = do
    a <- dummyName "a"
    let a' = var a
    pure $ UBuiltin (tyArr a' (tyOpt a')) Some
tyE0 (NBuiltin _ None) = do
    a <- dummyName "a"
    pure $ NBuiltin (tyOpt $ var a) None
tyE0 (UBuiltin l Parse) = do
    a <- dummyName "a"
    let a' = var a
    modify (mapClassVars (addC a (IsParseable, l)))
    pure $ UBuiltin (tyArr tyStr a') Parse
tyE0 (BBuiltin l Sprintf) = do
    a <- dummyName "a"
    let a' = var a
    modify (mapClassVars (addC a (IsPrintf, l)))
    pure $ BBuiltin (tyArr tyStr (tyArr a' tyStr)) Sprintf
tyE0 (UBuiltin _ (At i)) = do
    a <- dummyName "a"
    let a' = var a
        tyV = hkt tyVec a'
    pure $ UBuiltin (tyArr tyV a') (At i)
tyE0 (UBuiltin l (Select i)) = do
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
    modify (mapClassVars (addC a (HasField i b', l)))
    pure $ UBuiltin (tyArr a' b') (Select i)
tyE0 (UBuiltin l Dedup) = do
    a <- dummyName "a"
    let a' = var a
        fTy = tyArr (tyStream a') (tyStream a')
    modify (mapClassVars (addC a (IsEq, l)))
    pure $ UBuiltin fTy Dedup
tyE0 (UBuiltin _ Const) = do
    a <- dummyName "a"
    b <- dummyName "b"
    let a' = var a
        b' = var b
        fTy = tyArr a' (tyArr b' a')
    pure $ UBuiltin fTy Const
tyE0 (UBuiltin l CatMaybes) = do
    a <- dummyName "a"
    f <- higherOrder "f"
    let a' = var a
        f' = var f
        fTy = tyArr (hkt f' $ tyOpt a') (hkt f' a')
    modify (mapClassVars (addC f (Witherable, l)))
    pure $ UBuiltin fTy CatMaybes
tyE0 (BBuiltin l Filter) = do
    a <- dummyName "a"
    f <- higherOrder "f"
    let a' = var a
        f' = var f
        fTy = tyArr (tyArr a' tyBool) (tyArr (hkt f' a') (hkt f' a'))
    modify (mapClassVars (addC f (Witherable , l)))
    pure $ BBuiltin fTy Filter
tyE0 (BBuiltin l MapMaybe) = do
    a <- dummyName "a"
    b <- dummyName "b"
    f <- higherOrder "f"
    let a' = var a
        b' = var b
        f' = var f
        fTy = tyArr (tyArr a' (tyOpt b')) (tyArr (hkt f' a') (hkt f' b'))
    modify (mapClassVars (addC f (Witherable, l)))
    pure $ BBuiltin fTy MapMaybe
tyE0 (BBuiltin l Map) = do
    a <- dummyName "a"
    b <- dummyName "b"
    f <- higherOrder "f"
    let a' = var a
        b' = var b
        f' = var f
        fTy = tyArr (tyArr a' b') (tyArr (hkt f' a') (hkt f' b'))
    modify (mapClassVars (addC f (Functor, l)))
    pure $ BBuiltin fTy Map
-- (b -> a -> b) -> b -> Stream a -> b
tyE0 (TBuiltin l Fold) = do
    b <- dummyName "b"
    a <- dummyName "a"
    f <- higherOrder "f"
    let b' = var b
        a' = var a
        f' = var f
        fTy = tyArr (tyArr b' (tyArr a' b')) (tyArr b' (tyArr (hkt f' a') b'))
    modify (mapClassVars (addC f (Foldable, l)))
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
tyE0 (TBuiltin _ Option) = do
    b <- dummyName "b"
    a <- dummyName "a"
    let b' = var b
        a' = var a
        fTy = tyArr b' (tyArr (tyArr a' b') (tyArr (tyOpt a') b'))
    pure $ TBuiltin fTy Option
tyE0 (Implicit _ e) = do
    e' <- tyE0 e
    pure $ Implicit (tyStream (eLoc e')) e'
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
    modify (addVarEnv i a')
    e' <- tyE0 e
    pure $ Lam (tyArr a' (eLoc e')) (n $> a') e'
tyE0 (Let _ (n@(Name _ (Unique i) _), eϵ) e) = do
    eϵ' <- tyE0 eϵ
    let bTy = eLoc eϵ'
    modify (addVarEnv i bTy)
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
tyE0 Paren{} = desugar
tyE0 (OptionVal _ (Just e)) = do
    e' <- tyE0 e
    pure $ OptionVal (tyOpt $ eLoc e') (Just e')
tyE0 (OptionVal _ Nothing) = do
    a <- dummyName "a"
    let a' = var a
    pure $ OptionVal (tyOpt a') Nothing
tyE0 (Arr l v) | V.null v = do
    a <- dummyName "a"
    let a' = var a
    pure $ Arr (mkVec a') V.empty
               | otherwise = do
    v' <- traverse tyE0 v
    let x = V.head v'
    V.priorM_ (\y y' -> pushConstraint l (eLoc y) (eLoc y')) v'
    pure $ Arr (eLoc x) v'
tyE0 (Anchor l es) = do
    es' <- forM es $ \e -> do
        e' <- tyE0 e
        a <- dummyName "a"
        let a' = var a
        pushConstraint l (tyStream a') (eLoc e') $> e'
    pure $ Anchor (TyB Star TyUnit) es'
