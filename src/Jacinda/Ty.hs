{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Ty ( runTyM
                  , tyProgram
                  , match
                  , aT
                  -- * For debugging
                  , tyOf
                  ) where

import           Control.Exception          (Exception, throw)
import           Control.Monad              (zipWithM)
import           Control.Monad.Except       (liftEither, throwError)
import           Control.Monad.State.Strict (StateT, gets, modify, runState, runStateT)
import           Data.Bifunctor             (first, second)
import           Data.Foldable              (traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.Semigroup             ((<>))
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import qualified Data.Vector                as V
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST
import           Jacinda.Ty.Const
import           Prettyprinter              (Pretty (..), squotes, (<+>))

data Err a = UF a (T ()) (T ())
           | Doesn'tSatisfy a (T ()) C
           | IllScoped a (Nm a)
           | Ambiguous (T K) (E ())
           | Expected K K
           | IllScopedTyVar (TyName ())
           | MF (T ()) (T ())
           | Occ a (T ()) (T ())

instance Pretty a => Pretty (Err a) where
    pretty (UF l ty ty')           = pretty l <+> "could not unify type" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty')
    pretty (Doesn'tSatisfy l ty c) = pretty l <+> squotes (pretty ty) <+> "is not a member of class" <+> pretty c
    pretty (IllScoped l n)         = pretty l <+> squotes (pretty n) <+> "is not in scope."
    pretty (Ambiguous ty e)        = "type" <+> squotes (pretty ty) <+> "of" <+> squotes (pretty e) <+> "is ambiguous"
    pretty (Expected k0 k1)        = "Found kind" <+> pretty k0 <> ", expected kind" <+> pretty k1
    pretty (IllScopedTyVar n)      = "Type variable" <+> squotes (pretty n) <+> "is not in scope."
    pretty (MF t t')               = "Failed to match" <+> squotes (pretty t) <+> "against type" <+> squotes (pretty t')
    pretty (Occ l t t')            = pretty l <+> "occurs check failed when unifying type" <+> squotes (pretty t) <+> "with type" <+> squotes (pretty t')

instance Pretty a => Show (Err a) where show=show.pretty

instance (Typeable a, Pretty a) => Exception (Err a) where

-- solve, unify etc. THEN check that all constraints are satisfied?
-- (after accumulating classVar membership...)
data TyState a = TyState { maxU      :: !Int
                         , kindEnv   :: IM.IntMap K
                         , classVars :: IM.IntMap (S.Set (C, a))
                         , varEnv    :: IM.IntMap (T K)
                         }

mapMaxU :: (Int -> Int) -> TyState a -> TyState a
mapMaxU f (TyState u k c v) = TyState (f u) k c v

setMaxU :: Int -> TyState a -> TyState a
setMaxU i (TyState _ k c v) = TyState i k c v

mapCV :: (IM.IntMap (S.Set (C, a)) -> IM.IntMap (S.Set (C, a))) -> TyState a -> TyState a
mapCV f (TyState u k cvs v) = TyState u k (f cvs) v

addVarEnv :: Int -> T K -> TyState a -> TyState a
addVarEnv i ty (TyState u k cvs v) = TyState u k cvs (IM.insert i ty v)

addKindEnv :: Int -> K -> TyState a -> TyState a
addKindEnv i k (TyState u ks cvs v) = TyState u (IM.insert i k ks) cvs v

type TyM a = StateT (TyState a) (Either (Err a))

runTyM :: Int -> TyM a b -> Either (Err a) (b, Int)
runTyM i = fmap (second maxU) . flip runStateT (TyState i IM.empty IM.empty IM.empty)

type Subst a = IM.IntMap (T a)

aT :: Subst a -> T a -> T a
aT um ty'@(TyVar _ (Nm _ (U i) _)) =
    case IM.lookup i um of
        Just ty@TyVar{} -> aT (IM.delete i um) ty -- prevent cyclic lookups
        Just ty@Rho{}   -> aT (IM.delete i um) ty
        Just ty         -> aT um ty
        Nothing         -> ty'
aT um (Rho l n@(Nm _ (U i) _) rs) =
    case IM.lookup i um of
        Just ty@Rho{}   -> aT (IM.delete i um) ty
        Just ty@TyVar{} -> aT (IM.delete i um) ty
        Just ty         -> aT um ty
        Nothing         -> Rho l n (fmap (aT um) rs)
aT _ ty'@TyB{} = ty'
aT um (TyApp l ty ty') = TyApp l (aT um ty) (aT um ty')
aT um (TyArr l ty ty') = TyArr l (aT um ty) (aT um ty')
aT um (TyTup l tys)    = TyTup l (aT um <$> tys)

mguPrep :: l -> Subst a -> T a -> T a -> Either (Err l) (Subst a)
mguPrep l s t0 t1 =
    let t0' = aT s t0; t1' = aT s t1 in mgu l s t0' t1'

match :: T a -> T a -> Subst a
match t t' = either (throw :: Err () -> Subst a) id (maM t t')

maM :: T a -> T a -> Either (Err l) (Subst a)
maM (TyB _ b) (TyB _ b') | b == b' = Right mempty
maM (TyVar _ n) (TyVar _ n') | n == n' = Right mempty
maM (TyVar _ (Nm _ (U i) _)) t = Right (IM.singleton i t)
maM (TyArr _ t0 t1) (TyArr _ t0' t1') = (<>) <$> maM t0 t0' <*> maM t1' t1 -- TODO: I think <> is right
maM (TyTup _ ts) (TyTup _ ts')        = fmap mconcat (zipWithM maM ts ts')
maM (Rho _ n _) (Rho _ n' _) | n == n' = Right mempty
maM (Rho _ n rs) t@(Rho _ _ rs') | IM.keysSet rs' `IS.isSubsetOf` IM.keysSet rs = IM.insert (unU$unique n) t . mconcat <$> traverse (uncurry maM) (IM.elems (IM.intersectionWith (,) rs rs'))
maM (Rho _ n rs) t@(TyTup _ ts) | length ts >= fst (IM.findMax rs) = IM.insert (unU$unique n) t . mconcat <$> traverse (uncurry maM) [ (ts!!(i-1),tϵ) | (i,tϵ) <- IM.toList rs ]
maM t t'                              = Left $ MF (void t) (void t')

occ :: T a -> IS.IntSet
occ (TyVar _ (Nm _ (U i) _))  = IS.singleton i
occ TyB{}                     = IS.empty
occ (TyTup _ ts)              = foldMap occ ts
occ (TyApp _ t t')            = occ t <> occ t'
occ (TyArr _ t t')            = occ t <> occ t'
occ (Rho _ (Nm _ (U i) _) rs) = IS.insert i (foldMap occ (IM.elems rs))

mgu :: l -> Subst a -> T a -> T a -> Either (Err l) (Subst a)
mgu _ s (TyB _ b) (TyB _ b') | b == b' = Right s
mgu _ s (TyVar _ n) (TyVar _ n') | n == n' = Right s
mgu l s t t'@(TyVar _ (Nm _ (U k) _)) | k `IS.notMember` occ t = Right $ IM.insert k t s
                                      | otherwise = Left $ Occ l (void t') (void t)
mgu l s t@(TyVar _ (Nm _ (U k) _)) t' | k `IS.notMember` occ t' = Right $ IM.insert k t' s
                                      | otherwise = Left $ Occ l (void t) (void t')
mgu l s (TyArr _ t0 t1) (TyArr _ t0' t1')  = do {s0 <- mguPrep l s t0 t0'; mguPrep l s0 t1 t1'}
mgu l s (TyApp _ t0 t1) (TyApp _ t0' t1')  = do {s0 <- mguPrep l s t0 t0'; mguPrep l s0 t1 t1'}
mgu l s (TyTup _ ts) (TyTup _ ts') | length ts == length ts' = zS (mguPrep l) s ts ts'
mgu l s (Rho _ n rs) t'@(TyTup _ ts) | length ts >= fst (IM.findMax rs) = tS_ (\sϵ (i, tϵ) -> IM.insert (unU$unique n) t' <$> mguPrep l sϵ (ts!!(i-1)) tϵ) s (IM.toList rs)
mgu l s t@TyTup{} t'@Rho{} = mgu l s t' t
mgu l s (Rho k n rs) (Rho _ n' rs') = do
    rss <- tS_ (\sϵ (t0,t1) -> mguPrep l sϵ t0 t1) s $ IM.elems $ IM.intersectionWith (,) rs rs'
    pure (IM.insert (unU$unique n) (Rho k n' (rs <> rs')) rss)
mgu l _ t t' = Left $ UF l (void t) (void t')

tS_ :: Monad m => (Subst a -> b -> m (Subst a)) -> Subst a -> [b] -> m (Subst a)
tS_ _ s []     = pure s
tS_ f s (t:ts) = do{next <- f s t; tS_ f next ts}

zS _ s [] _           = pure s
zS _ s _ []           = pure s
zS op s (x:xs) (y:ys) = do{next <- op s x y; zS op next xs ys}

substInt :: IM.IntMap (T a) -> Int -> Maybe (T a)
substInt tys k =
    case IM.lookup k tys of
        Just ty'@TyVar{}       -> Just $ aT (IM.delete k tys) ty'
        Just (TyApp l ty0 ty1) -> Just $ let tys' = IM.delete k tys in TyApp l (aT tys' ty0) (aT tys' ty1)
        Just (TyArr l ty0 ty1) -> Just $ let tys' = IM.delete k tys in TyArr l (aT tys' ty0) (aT tys' ty1)
        Just (TyTup l tysϵ)    -> Just $ let tys' = IM.delete k tys in TyTup l (aT tys' <$> tysϵ)
        Just ty'               -> Just ty'
        Nothing                -> Nothing

freshName :: T.Text -> K -> TyM a (Nm K)
freshName n k = do
    st <- gets maxU
    Nm n (U $ st+1) k
        <$ modify (mapMaxU (+1))

namek :: Nm K -> TyM a (Nm K)
namek n =
    modify (addKindEnv (unU$unique n) (loc n)) $> n

higherOrder :: T.Text -> TyM a (Nm K)
higherOrder t = freshName t (KArr Star Star) >>= namek

-- of kind 'Star'
dummyName :: T.Text -> TyM a (Nm K)
dummyName n = freshName n Star >>= namek

addC :: Ord a => Nm b -> (C, a) -> IM.IntMap (S.Set (C, a)) -> IM.IntMap (S.Set (C, a))
addC (Nm _ (U i) _) c = IM.alter (Just . go) i where
    go Nothing   = S.singleton c
    go (Just cs) = S.insert c cs

-- | arguments assumed to have kind 'Star'
tyArr :: T K -> T K -> T K
tyArr = TyArr Star

var :: Nm K -> T K
var = TyVar Star

isStar :: K -> TyM a ()
isStar Star = pure ()
isStar k    = throwError $ Expected k Star

liftCloneTy :: T a -> TyM b (T a)
liftCloneTy ty = do
    i <- gets maxU
    let (ty', (j, iMaps)) = cloneTy i ty
    -- FIXME: clone/propagate constraints
    ty' <$ modify (setMaxU j)

cloneTy :: Int -> T a -> (T a, (Int, IM.IntMap U))
cloneTy i ty = flip runState (i, IM.empty) $ cloneTyM ty
    where cloneTyM (TyVar l (Nm n (U j) l')) = do
                st <- gets snd
                case IM.lookup j st of
                    Just k -> pure (TyVar l (Nm n k l'))
                    Nothing -> do
                        k <- gets fst
                        let j' = U$k+1
                        TyVar l (Nm n j' l') <$ modify (\(u, s) -> (u+1, IM.insert j j' s))
          cloneTyM (TyArr l tyϵ ty')               = TyArr l <$> cloneTyM tyϵ <*> cloneTyM ty'
          cloneTyM (TyApp l tyϵ ty')               = TyApp l <$> cloneTyM tyϵ <*> cloneTyM ty'
          cloneTyM (TyTup l tys)                   = TyTup l <$> traverse cloneTyM tys
          cloneTyM tyϵ@TyB{}                       = pure tyϵ

kind :: T K -> TyM a ()
kind (TyB Star TyStr)                  = pure ()
kind (TyB Star TyInteger)              = pure ()
kind (TyB Star TyFloat)                = pure ()
kind (TyB (KArr Star Star) TyStream)   = pure ()
kind (TyB (KArr Star Star) TyOption)   = pure ()
kind (TyB Star TyBool)                 = pure ()
kind (TyB (KArr Star Star) TyVec)      = pure ()
kind (TyB Star TyUnit)                 = pure ()
kind (TyB k TyStr)                     = throwError $ Expected Star k
kind (TyB k TyInteger)                 = throwError $ Expected Star k
kind (TyB k TyFloat)                   = throwError $ Expected Star k
kind (TyB k TyUnit)                    = throwError $ Expected Star k
kind (TyB k TyBool)                    = throwError $ Expected Star k
kind (TyB k TyOption)                  = throwError $ Expected (KArr Star Star) k
kind (TyB k TyStream)                  = throwError $ Expected (KArr Star Star) k
kind (TyB k TyVec)                     = throwError $ Expected (KArr Star Star) k
kind (TyVar _ n@(Nm _ (U i) _)) = do
    preK <- gets (IM.lookup i . kindEnv)
    case preK of
        Just{}  -> pure ()
        Nothing -> throwError $ IllScopedTyVar (void n)
kind (TyTup Star tys) =
    traverse_  (isStar.tLoc) tys
kind (TyTup k _) = throwError $ Expected Star k
kind (TyArr Star ty0 ty1) =
    isStar (tLoc ty0) *>
    isStar (tLoc ty1)
kind (TyArr k _ _) = throwError $ Expected Star k
kind (TyApp k1 ty0 ty1) = do
    case tLoc ty0 of
        (KArr k0 k1') | k0 == tLoc ty1 && k1' == k1 -> pure ()
                      | k0 == tLoc ty1 -> throwError $ Expected k1' k1
                      | otherwise        -> throwError $ Expected (tLoc ty1) k0
        k0                               -> throwError $ Expected (KArr Star Star) k0

checkType :: Ord a => T K -> (C, a) -> TyM a ()
checkType TyVar{} _                            = pure ()
checkType (TyB _ TyR) (IsSemigroup, _)         = pure ()
checkType (TyB _ TyStr) (IsSemigroup, _)       = pure ()
checkType (TyB _ TyInteger) (IsSemigroup, _)   = pure ()
checkType (TyB _ TyInteger) (IsNum, _)         = pure ()
checkType (TyB _ TyInteger) (IsOrd, _)         = pure ()
checkType (TyB _ TyInteger) (IsEq, _)          = pure ()
checkType (TyB _ TyInteger) (IsParse, _)       = pure ()
checkType (TyB _ TyFloat) (IsParse, _)         = pure ()
checkType ty (IsParse, l)                      = throwError $ Doesn'tSatisfy l (void ty) IsParse
checkType (TyB _ TyFloat) (IsSemigroup, _)     = pure ()
checkType (TyB _ TyFloat) (IsNum, _)           = pure ()
checkType (TyB _ TyFloat) (IsOrd, _)           = pure ()
checkType (TyB _ TyFloat) (IsEq, _)            = pure ()
checkType (TyB _ TyBool) (IsEq, _)             = pure ()
checkType (TyB _ TyStr) (IsEq, _)              = pure ()
checkType ty@(TyB _ TyStr) (c@IsOrd, l)        = throwError $ Doesn'tSatisfy l (void ty) c
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
checkType (Rho _ _ rs) (IsPrintf, l)           = traverse_ (`checkType` (IsPrintf, l)) (IM.elems rs)
checkType ty (c@IsPrintf, l)                   = throwError $ Doesn'tSatisfy l (void ty) c

checkClass :: Ord a
           => IM.IntMap (T K) -- ^ Unification result
           -> Int
           -> S.Set (C, a)
           -> TyM a ()
checkClass tys i cs = {-# SCC "checkClass" #-}
    case substInt tys i of
        Just ty -> traverse_ (checkType ty) (S.toList cs)
        Nothing -> pure () -- FIXME: we need to check that the var is well-kinded for constraint

lookupVar :: Nm a -> TyM a (T K)
lookupVar n@(Nm _ (U i) l) = do
    st <- gets varEnv
    case IM.lookup i st of
        Just ty -> pure ty -- liftCloneTy ty
        Nothing -> throwError $ IllScoped l n

tyOf :: Ord a => E a -> TyM a (T K)
tyOf = fmap eLoc . tyE

tyDS :: Ord a => Subst K -> D a -> TyM a (D (T K), Subst K)
tyDS s (SetFS bs) = pure (SetFS bs, s)
tyDS s FlushDecl  = pure (FlushDecl, s)
tyDS s (FunDecl n@(Nm _ (U i) _) [] e) = do
    (e', s') <- tyES s e
    let t=eLoc e'
    modify (addVarEnv i t) $> (FunDecl (n$>t) [] e', s')
tyDS _ FunDecl{}   = error "Internal error. Should have been desugared by now."

isAmbiguous :: T K -> Bool
isAmbiguous TyVar{}          = True
isAmbiguous (TyArr _ ty ty') = isAmbiguous ty || isAmbiguous ty'
isAmbiguous (TyApp _ ty ty') = isAmbiguous ty || isAmbiguous ty'
isAmbiguous (TyTup _ tys)    = any isAmbiguous tys
isAmbiguous TyB{}            = False
isAmbiguous Rho{}            = True

checkAmb :: E (T K) -> TyM a ()
checkAmb e@(BB ty _) | isAmbiguous ty = throwError $ Ambiguous ty (void e)
checkAmb TB{} = pure () -- don't fail on ternary builtins, we don't need it anyway... better error messages
checkAmb e@(UB ty _) | isAmbiguous ty = throwError $ Ambiguous ty (void e)
checkAmb (Implicit _ e') = checkAmb e'
checkAmb (Guarded _ p e') = checkAmb p *> checkAmb e'
checkAmb (EApp _ e' e'') = checkAmb e' *> checkAmb e'' -- more precise errors
checkAmb (Tup _ es) = traverse_ checkAmb es
checkAmb e@(Arr ty _) | isAmbiguous ty = throwError $ Ambiguous ty (void e)
checkAmb e@(Var ty _) | isAmbiguous ty = throwError $ Ambiguous ty (void e)
checkAmb (Let _ bs e) = traverse_ checkAmb [e, snd bs]
checkAmb (Lam _ _ e) = checkAmb e -- I think
checkAmb _ = pure ()

tS _ s []     = pure ([], s)
tS f s (t:ts) = do{(x, next) <- f s t; first (x:) <$> tS f next ts}

tyProgram :: Ord a => Program a -> TyM a (Program (T K))
tyProgram (Program ds e) = do
    (ds', s0) <- tS tyDS mempty ds
    (e', s1) <- tyES s0 e
    toCheck <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass s1)) toCheck
    let res = {-# SCC "aT" #-} fmap (aT s1) (Program ds' e')
    checkAmb (expr res) $> res

tyNumOp :: Ord a => a -> TyM a (T K)
tyNumOp l = do
    m <- dummyName "m"
    modify (mapCV (addC m (IsNum, l)))
    let m' = var m
    pure $ tyArr m' (tyArr m' m')

tySemiOp :: Ord a => a -> TyM a (T K)
tySemiOp l = do
    m <- dummyName "m"
    modify (mapCV (addC m (IsSemigroup, l)))
    let m' = var m
    pure $ tyArr m' (tyArr m' m')

tyOrd :: Ord a => a -> TyM a (T K)
tyOrd l = do
    a <- dummyName "a"
    modify (mapCV (addC a (IsOrd, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' tyB)

tyEq :: Ord a => a -> TyM a (T K)
tyEq l = do
    a <- dummyName "a"
    modify (mapCV (addC a (IsEq, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' tyB)

-- min/max
tyM :: Ord a => a -> TyM a (T K)
tyM l = do
    a <- dummyName "a"
    modify (mapCV (addC a (IsOrd, l)))
    let a' = var a
    pure $ tyArr a' (tyArr a' a')

desugar :: a
desugar = error "Should have been de-sugared in an earlier stage!"

tyE :: Ord a => E a -> TyM a (E (T K))
tyE e = do
    (e', s) <- tyES mempty e
    cvs <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass s)) cvs
    pure (fmap (aT s) e')

tyES :: Ord a => Subst K -> E a -> TyM a (E (T K), Subst K)
tyES s (BoolLit _ b)      = pure (BoolLit tyB b, s)
tyES s (IntLit _ i)       = pure (IntLit tyI i, s)
tyES s (FloatLit _ f)     = pure (FloatLit tyF f, s)
tyES s (StrLit _ str)     = pure (StrLit tyStr str, s)
tyES s (RegexLit _ rr)    = pure (RegexLit tyR rr, s)
tyES s (Column _ i)       = pure (Column (tyStream tyStr) i, s)
tyES s (IParseCol _ i)    = pure (IParseCol (tyStream tyI) i, s)
tyES s (FParseCol _ i)    = pure (FParseCol (tyStream tyF) i, s)
tyES s (Field _ i)        = pure (Field tyStr i, s)
tyES s LastField{}        = pure (LastField tyStr, s)
tyES s AllField{}         = pure (AllField tyStr, s)
tyES s AllColumn{}        = pure (AllColumn (tyStream tyStr), s)
tyES s (NB _ Ix)    = pure (NB tyI Ix, s)
tyES s (NB _ Fp)    = pure (NB tyStr Fp, s)
tyES s (NB _ Nf)    = pure (NB tyI Nf, s)
tyES s (BB l Plus)  = do {t <- tySemiOp l; pure (BB t Plus, s)}
tyES s (BB l Minus) = do {t <- tyNumOp l; pure (BB t Minus, s)}
tyES s (BB l Times) = do {t <- tyNumOp l; pure (BB t Times, s)}
tyES s (BB l Exp)   = do {t <- tyNumOp l; pure (BB t Exp, s)}
tyES s (BB l Gt)    = do {t <- tyOrd l; pure (BB t Gt, s)}
tyES s (BB l Lt)    = do {t <- tyOrd l; pure (BB t Lt, s)}
tyES s (BB l Geq)   = do {t <- tyOrd l; pure (BB t Geq, s)}
tyES s (BB l Leq)   = do {t <- tyOrd l; pure (BB t Leq, s)}
tyES s (BB l Eq)    = do {t <- tyEq l; pure (BB t Eq, s)}
tyES s (BB l Neq)   = do {t <- tyEq l; pure (BB t Neq, s)}
tyES s (BB l Min)   = do {t <- tyM l; pure (BB t Min, s)}
tyES s (BB l Max)   = do {t <- tyM l; pure (BB t Max, s)}
tyES s (BB _ Split) = pure (BB (tyArr tyStr (tyArr tyR (tyV tyStr))) Split, s)
tyES s (BB _ Splitc) = pure (BB (tyArr tyStr (tyArr tyStr (tyV tyStr))) Splitc, s)
tyES s (BB _ Matches) = pure (BB (tyArr tyStr (tyArr tyR tyB)) Matches, s)
tyES s (BB _ NotMatches) = pure (BB (tyArr tyStr (tyArr tyR tyB)) NotMatches, s)
tyES s (UB _ Tally) = pure (UB (tyArr tyStr tyI) Tally, s)
tyES s (BB _ Div) = pure (BB (tyArr tyF (tyArr tyF tyF)) Div, s)
tyES s (UB _ Not) = pure (UB (tyArr tyB tyB) Not, s)
tyES s (BB _ And) = pure (BB (tyArr tyB (tyArr tyB tyB)) And, s)
tyES s (BB _ Or) = pure (BB (tyArr tyB (tyArr tyB tyB)) Or, s)
tyES s (BB _ Match) = pure (BB (tyArr tyStr (tyArr tyR (tyOpt $ TyTup Star [tyI, tyI]))) Match, s)
tyES s (TB _ Substr) = pure (TB (tyArr tyStr (tyArr tyI (tyArr tyI tyStr))) Substr, s)
tyES s (UB _ IParse) = pure (UB (tyArr tyStr tyI) IParse, s)
tyES s (UB _ FParse) = pure (UB (tyArr tyStr tyF) FParse, s)
tyES s (UB _ Floor) = pure (UB (tyArr tyF tyI) Floor, s)
tyES s (UB _ Ceiling) = pure (UB (tyArr tyF tyI) Ceiling, s)
tyES s (UB _ TallyList) = do {a <- var <$> dummyName "a"; pure (UB (tyArr a tyI) TallyList, s)}
tyES s (UB l Negate) = do {a <- dummyName "a"; modify (mapCV (addC a (IsNum, l))); let a'=var a in pure (UB (tyArr a' a') Negate, s)}
tyES s (UB _ Some) = do {a <- var <$> dummyName "a"; pure (UB (tyArr a (tyOpt a)) Some, s)}
tyES s (NB _ None) = do {a <- dummyName "a"; pure (NB (tyOpt (var a)) None, s)}
tyES s (ParseCol l i) = do {a <- dummyName "a"; modify (mapCV (addC a (IsParse, l))); pure (ParseCol (tyStream (var a)) i, s)}
tyES s (UB l Parse) = do {a <- dummyName "a"; modify (mapCV (addC a (IsParse, l))); pure (UB (tyArr tyStr (var a)) Parse, s)}
tyES s (BB l Sprintf) = do {a <- dummyName "a"; modify (mapCV (addC a (IsPrintf, l))); pure (BB (tyArr tyStr (tyArr (var a) tyStr)) Sprintf, s)}
tyES s (BB l DedupOn) = do {a <- var <$> dummyName "a"; b <- dummyName "b"; modify (mapCV (addC b (IsEq, l))); let b'=var b in pure (BB (tyArr (tyArr a b') (tyArr (tyStream a) (tyStream b'))) DedupOn, s)}
tyES s (UB _ (At i)) = do {a <- var <$> dummyName "a"; pure (UB (tyArr (tyV a) a) (At i), s)}
tyES s (UB l Dedup) = do {a <- dummyName "a"; modify (mapCV (addC a (IsEq, l))); let sA=tyStream (var a) in pure (UB (tyArr sA sA) Dedup, s)}
tyES s (UB _ Const) = do {a <- var <$> dummyName "a"; b <- var <$> dummyName "b"; pure (UB (tyArr a (tyArr b a)) Const, s)}
tyES s (UB l CatMaybes) = do {a <- dummyName "a"; f <- higherOrder "f"; modify (mapCV (addC f (Witherable, l))); let a'=var a; f'=var f in pure (UB (tyArr (hkt f' (tyOpt a')) (hkt f' a')) CatMaybes, s)}
tyES s (BB l Filter) = do {a <- dummyName "a"; f <- higherOrder "f"; modify (mapCV (addC f (Witherable, l))); let a'=var a; f'=var f; w=hkt f' a' in pure (BB (tyArr (tyArr a' tyB) (tyArr w w)) Filter, s)}
tyES s (UB _ (Select i)) = do
    ρ <- dummyName "ρ"; a <- var <$> dummyName "a"
    pure (UB (tyArr (Rho Star ρ (IM.singleton i a)) a) (Select i), s)
tyES s (BB l MapMaybe) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    f <- higherOrder "f"
    modify (mapCV (addC f (Witherable, l)))
    let f'=var f
    pure (BB (tyArr (tyArr a (tyOpt b)) (tyArr (hkt f' a) (hkt f' b))) MapMaybe, s)
tyES s (BB l Map) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    f <- higherOrder "f"
    let f'=var f
    modify (mapCV (addC f (Functor, l)))
    pure (BB (tyArr (tyArr a b) (tyArr (hkt f' a) (hkt f' b))) Map, s)
tyES s (TB l Fold) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    f <- higherOrder "f"
    let f'=var f
    modify (mapCV (addC f (Foldable, l)))
    pure (TB (tyArr (tyArr b (tyArr a b)) (tyArr b (tyArr (hkt f' a) b))) Fold, s)
tyES s (BB l Fold1) = do
    a <- var <$> dummyName "a"
    f <- higherOrder "f"
    let f'=var f
    modify (mapCV (addC f (Foldable, l)))
    pure (BB (tyArr (tyArr a (tyArr a a)) (tyArr (hkt f' a) a)) Fold1, s)
tyES s (TB _ Captures) = pure (TB (tyArr tyStr (tyArr tyI (tyArr tyR (tyOpt tyStr)))) Captures, s)
tyES s (BB _ Prior) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    pure (BB (tyArr (tyArr a (tyArr a b)) (tyArr (tyStream a) (tyStream b))) Prior, s)
tyES s (TB _ ZipW) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"; c <- var <$> dummyName "c"
    pure (TB (tyArr (tyArr a (tyArr b c)) (tyArr (tyStream a) (tyArr (tyStream b) (tyStream c)))) ZipW, s)
tyES s (TB _ Scan) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    pure (TB (tyArr (tyArr b (tyArr a b)) (tyArr b (tyArr (tyStream a) (tyStream b)))) Scan, s)
tyES s (TB _ Option) = do
    a <- var <$> dummyName "a"; b <- var <$> dummyName "b"
    pure (TB (tyArr b (tyArr (tyArr a b) (tyArr (tyOpt a) b))) Option, s)
tyES s (TB _ AllCaptures) = pure (TB (tyArr tyStr (tyArr tyI (tyArr tyR (tyV tyStr)))) AllCaptures, s)
tyES s (Implicit _ e) = do {(e',s') <- tyES s e; pure (Implicit (tyStream (eLoc e')) e', s')}
tyES s (Guarded l e se) = do
    (se', s0) <- tyES s se
    (e', s1) <- tyES s0 e
    s2 <- liftEither $ mguPrep l s1 tyB (eLoc e')
    pure (Guarded (tyStream (eLoc se')) e' se', s2)
tyES s (EApp l e0 e1)     = do
    a <- dummyName "a"; b <- dummyName "b"
    let a'=var a; b'=var b; e0Ty=tyArr a' b'
    (e0', s0) <- tyES s e0
    (e1', s1) <- tyES s0 e1
    s2 <- liftEither $ mguPrep l s1 (eLoc e0') e0Ty
    s3 <- liftEither $ mguPrep l s2 (eLoc e1') a'
    pure (EApp b' e0' e1', s3)
tyES s (Lam _ n@(Nm _ (U i) _) e) = do
    a <- var <$> dummyName "a"
    modify (addVarEnv i a)
    (e', s') <- tyES s e
    pure (Lam (tyArr a (eLoc e')) (n$>a) e', s')
tyES s (Let _ (n@(Nm _ (U i) _), eϵ) e) = do
    (eϵ', s0) <- tyES s eϵ
    let bTy=eLoc eϵ'
    modify (addVarEnv i bTy)
    (e', s1) <- tyES s0 e
    pure (Let (eLoc e') (n$>bTy, eϵ') e', s1)
tyES s (Tup _ es) = do {(es', s') <- tS tyES s es; pure (Tup (TyTup Star (fmap eLoc es')) es', s')}
tyES s (Var _ n) = do {t <- lookupVar n; pure (Var t (n$>t), s)}
tyES s (OptionVal _ (Just e)) = do {(e', s') <- tyES s e; pure (OptionVal (tyOpt (eLoc e')) (Just e'), s')}
tyES s (OptionVal _ Nothing) = do {a <- var <$> dummyName "a"; pure (OptionVal (tyOpt a) Nothing, s)}
tyES s (Arr l v) | V.null v = do
    a <- var <$> dummyName "a"
    pure (Arr (tyV a) V.empty, s)
                 | otherwise = do
    (v',s0) <- tS tyES s (V.toList v)
    let vt=fmap eLoc v'
    s1 <- liftEither $ zS (mguPrep l) s0 vt (tail vt)
    pure (Arr (head vt) (V.fromList v'), s1)
tyES s (Cond l p e0 e1) = do
    (p', s0) <- tyES s p
    (e0', s1) <- tyES s0 e0
    (e1', s2) <- tyES s1 e1
    let t=eLoc e0'
    s3 <- liftEither $ mguPrep l s2 tyB (eLoc p')
    s4 <- liftEither $ mguPrep l s3 t (eLoc e1')
    pure (Cond t p' e0' e1', s4)
tyES s (Anchor l es) = do
    (es', s') <- tS (\sϵ e -> do {(e',s0) <- tyES sϵ e; a <- var <$> dummyName "a"; s1 <- liftEither $ mguPrep l s0 (tyStream a) (eLoc e'); pure (e', s1)}) s es
    pure (Anchor (TyB Star TyUnit) es', s')
tyES _ RegexCompiled{} = error "Regex should not be compiled at this stage."
tyES _ Dfn{} = desugar; tyES _ ResVar{} = desugar; tyES _ Paren{} = desugar
