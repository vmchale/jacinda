{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ty ( Subst
          , runTyM
          , tyP
          , match
          , aT
          -- * For debugging
          , tyOf
          ) where

import           A
import           Control.Exception          (Exception, throw)
import           Control.Monad              (zipWithM)
import           Control.Monad.Except       (liftEither, throwError)
import           Control.Monad.State.Strict (StateT, gets, modify, runState, runStateT)
import           Data.Bifunctor             (first, second)
import           Data.Foldable              (traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import qualified Data.Vector                as V
import           Nm
import           Prettyprinter              (Pretty (..), squotes, (<+>))
import           Ty.Const
import           U

data Err a = UF a T T
           | Doesn'tSatisfy a T C
           | IllScoped a (Nm a)
           | Ambiguous T (E ())
           | IllScopedTyVar (TyName ())
           | MF T T
           | Occ a T T

instance Pretty a => Pretty (Err a) where
    pretty (UF l ty ty')           = pretty l <+> "could not unify type" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty')
    pretty (Doesn'tSatisfy l ty c) = pretty l <+> squotes (pretty ty) <+> "is not a member of class" <+> pretty c
    pretty (IllScoped l n)         = pretty l <+> squotes (pretty n) <+> "is not in scope."
    pretty (Ambiguous ty e)        = "type" <+> squotes (pretty ty) <+> "of" <+> squotes (pretty e) <+> "is ambiguous"
    pretty (IllScopedTyVar n)      = "Type variable" <+> squotes (pretty n) <+> "is not in scope."
    pretty (MF t t')               = "Failed to match" <+> squotes (pretty t) <+> "against type" <+> squotes (pretty t')
    pretty (Occ l t t')            = pretty l <+> "occurs check failed when unifying type" <+> squotes (pretty t) <+> "with type" <+> squotes (pretty t')

instance Pretty a => Show (Err a) where show=show.pretty

instance (Typeable a, Pretty a) => Exception (Err a) where

data TyState a = TyState { maxU      :: !Int
                         , classVars :: IM.IntMap (S.Set (C, a))
                         , varEnv    :: IM.IntMap T
                         }

mapMaxU :: (Int -> Int) -> TyState a -> TyState a
mapMaxU f (TyState u c v) = TyState (f u) c v

setMaxU :: Int -> TyState a -> TyState a
setMaxU i (TyState _ c v) = TyState i c v

mapCV :: (IM.IntMap (S.Set (C, a)) -> IM.IntMap (S.Set (C, a))) -> TyState a -> TyState a
mapCV f (TyState u cvs v) = TyState u (f cvs) v

addVarEnv :: Int -> T -> TyState a -> TyState a
addVarEnv i ty (TyState u cvs v) = TyState u cvs (IM.insert i ty v)

type TyM a = StateT (TyState a) (Either (Err a))

runTyM :: Int -> TyM a b -> Either (Err a) (b, Int)
runTyM i = fmap (second maxU) . flip runStateT (TyState i IM.empty IM.empty)

type Subst = IM.IntMap T

aT :: Subst -> T -> T
aT um ty'@(TyVar (Nm _ (U i) _)) =
    case IM.lookup i um of
        Just ty@TyVar{} -> aT (IM.delete i um) ty -- prevent cyclic lookups
        Just ty@Rho{}   -> aT (IM.delete i um) ty
        Just ty         -> aT um ty
        Nothing         -> ty'
aT um (Rho n@(Nm _ (U i) _) rs) =
    case IM.lookup i um of
        Just ty@Rho{}   -> aT (IM.delete i um) ty
        Just ty@TyVar{} -> aT (IM.delete i um) ty
        Just ty         -> aT um ty
        Nothing         -> Rho n (fmap (aT um) rs)
aT _ ty'@TyB{} = ty'
aT um (ty:$ty') = aT um ty :$ aT um ty'
aT um (TyArr ty ty') = TyArr (aT um ty) (aT um ty')
aT um (TyTup tys)    = TyTup (aT um <$> tys)

mguPrep :: l -> Subst -> T -> T -> Either (Err l) Subst
mguPrep l s t0 t1 =
    let t0' = aT s t0; t1' = aT s t1 in mgu l s t0' t1'

match :: T -> T -> Subst
match t t' = either (throw :: Err () -> Subst) id (maM t t')

maM :: T -> T -> Either (Err l) Subst
maM (TyB b) (TyB b') | b == b' = Right mempty
maM (TyVar n) (TyVar n') | n == n' = Right mempty
maM (TyVar (Nm _ (U i) _)) t = Right (IM.singleton i t)
maM (TyArr t0 t1) (TyArr t0' t1') = (<>) <$> maM t0 t0' <*> maM t1' t1 -- TODO: I think <> is right
maM (TyTup ts) (TyTup ts')        = fmap mconcat (zipWithM maM ts ts')
maM (Rho n _) (Rho n' _) | n == n' = Right mempty
maM (Rho n rs) t@(Rho _ rs') | IM.keysSet rs' `IS.isSubsetOf` IM.keysSet rs = IM.insert (unU$unique n) t . mconcat <$> traverse (uncurry maM) (IM.elems (IM.intersectionWith (,) rs rs'))
maM (Rho n rs) t@(TyTup ts) | length ts >= fst (IM.findMax rs) = IM.insert (unU$unique n) t . mconcat <$> traverse (uncurry maM) [ (ts!!(i-1),tϵ) | (i,tϵ) <- IM.toList rs ]
maM t t'                              = Left $ MF t t'

occ :: T -> IS.IntSet
occ (TyVar (Nm _ (U i) _))  = IS.singleton i
occ TyB{}                   = IS.empty
occ (TyTup ts)              = foldMap occ ts
occ (t:$t')                 = occ t <> occ t'
occ (TyArr t t')            = occ t <> occ t'
occ (Rho (Nm _ (U i) _) rs) = IS.insert i (foldMap occ (IM.elems rs))

mgu :: l -> Subst -> T -> T -> Either (Err l) Subst
mgu _ s (TyB b) (TyB b') | b == b' = Right s
mgu _ s (TyVar n) (TyVar n') | n == n' = Right s
mgu l s t t'@(TyVar (Nm _ (U k) _)) | k `IS.notMember` occ t = Right $ IM.insert k t s
                                      | otherwise = Left $ Occ l t' t
mgu l s t@(TyVar (Nm _ (U k) _)) t' | k `IS.notMember` occ t' = Right $ IM.insert k t' s
                                      | otherwise = Left $ Occ l t t'
mgu l s (TyArr t0 t1) (TyArr t0' t1')  = do {s0 <- mguPrep l s t0 t0'; mguPrep l s0 t1 t1'}
mgu l s (t0:$t1) (t0':$t1')            = do {s0 <- mguPrep l s t0 t0'; mguPrep l s0 t1 t1'}
mgu l s (TyTup ts) (TyTup ts') | length ts == length ts' = zS (mguPrep l) s ts ts'
mgu l s (Rho n rs) t'@(TyTup ts) | length ts >= fst (IM.findMax rs) && fst (IM.findMin rs) > 0 = tS_ (\sϵ (i, tϵ) -> IM.insert (unU$unique n) t' <$> mguPrep l sϵ (ts!!(i-1)) tϵ) s (IM.toList rs)
mgu l s t@TyTup{} t'@Rho{} = mgu l s t' t
mgu l s (Rho n rs) (Rho n' rs') = do
    rss <- tS_ (\sϵ (t0,t1) -> mguPrep l sϵ t0 t1) s $ IM.elems $ IM.intersectionWith (,) rs rs'
    pure (IM.insert (unU$unique n) (Rho n' (rs <> rs')) rss)
mgu l _ t t' = Left $ UF l t t'

tS_ :: Monad m => (Subst -> b -> m Subst) -> Subst -> [b] -> m Subst
tS_ _ s []     = pure s
tS_ f s (t:ts) = do {next <- f s t; tS_ f next ts}

zS _ s [] _           = pure s
zS _ s _ []           = pure s
zS op s (x:xs) (y:ys) = do {next <- op s x y; zS op next xs ys}

substInt :: IM.IntMap T -> Int -> Maybe T
substInt tys k =
    case IM.lookup k tys of
        Just ty'@TyVar{}     -> Just $ aT (IM.delete k tys) ty'
        Just (ty0:$ty1)      -> Just $ let tys'=IM.delete k tys in aT tys' ty0 :$ aT tys' ty1
        Just (TyArr ty0 ty1) -> Just $ let tys'=IM.delete k tys in TyArr (aT tys' ty0) (aT tys' ty1)
        Just (TyTup tysϵ)    -> Just $ let tys'=IM.delete k tys in TyTup (aT tys' <$> tysϵ)
        Just ty'             -> Just ty'
        Nothing              -> Nothing

freshN :: T.Text -> TyM a (Nm ())
freshN n = do
    st <- gets maxU
    Nm n (U $ st+1) ()
        <$ modify (mapMaxU (+1))

freshTV :: T.Text -> TyM a T
freshTV=fmap var.freshN

addC :: Ord a => Nm b -> (C, a) -> IM.IntMap (S.Set (C, a)) -> IM.IntMap (S.Set (C, a))
addC (Nm _ (U i) _) c = IM.alter (Just . go) i where
    go Nothing   = S.singleton c
    go (Just cs) = S.insert c cs

tyArr :: T -> T -> T
tyArr = TyArr

var :: Nm () -> T
var = TyVar

liftCloneTy :: T -> TyM b T
liftCloneTy ty = do
    i <- gets maxU
    let (ty', (j, iMaps)) = cloneTy i ty
    -- FIXME: clone/propagate constraints
    ty' <$ modify (setMaxU j)

cloneTy :: Int -> T -> (T, (Int, IM.IntMap U))
cloneTy i ty = flip runState (i, IM.empty) $ cloneTyM ty
    where cloneTyM (TyVar (Nm n (U j) l')) = do
                st <- gets snd
                case IM.lookup j st of
                    Just k -> pure (TyVar (Nm n k l'))
                    Nothing -> do
                        k <- gets fst
                        let j' = U$k+1
                        TyVar (Nm n j' l') <$ modify (\(u, s) -> (u+1, IM.insert j j' s))
          cloneTyM (TyArr tyϵ ty')               = TyArr <$> cloneTyM tyϵ <*> cloneTyM ty'
          cloneTyM (tyϵ:$ty')                    = (:$) <$> cloneTyM tyϵ <*> cloneTyM ty'
          cloneTyM (TyTup tys)                   = TyTup <$> traverse cloneTyM tys
          cloneTyM tyϵ@TyB{}                     = pure tyϵ

checkType :: Ord a => T -> (C, a) -> TyM a ()
checkType TyVar{} _                      = pure ()
checkType (TyB TyStr) (IsSemigroup, _)   = pure ()
checkType (TyB TyI) (IsSemigroup, _)     = pure ()
checkType (TyB TyFloat) (IsSemigroup, _) = pure ()
checkType (TyB TyI) (IsNum, _)           = pure ()
checkType (TyB TyFloat) (IsNum, _)       = pure ()
checkType (TyB TyI) (IsEq, _)            = pure ()
checkType (TyB TyFloat) (IsEq, _)        = pure ()
checkType (TyB TyBool) (IsEq, _)         = pure ()
checkType (TyB TyStr) (IsEq, _)          = pure ()
checkType (TyTup tys) (c@IsEq, l)        = traverse_ (`checkType` (c, l)) tys
checkType (Rho _ rs) (c@IsEq, l)         = traverse_ (`checkType` (c, l)) (IM.elems rs)
checkType (TyB TyVec:$ty) (c@IsEq, l)    = checkType ty (c, l)
checkType (TyB TyOption:$ty) (c@IsEq, l) = checkType ty (c, l)
checkType (TyB TyI) (IsParse, _)         = pure ()
checkType (TyB TyFloat) (IsParse, _)     = pure ()
checkType (TyB TyFloat) (IsOrd, _)       = pure ()
checkType (TyB TyI) (IsOrd, _)           = pure ()
checkType (TyB TyStr) (IsOrd, _)         = pure ()
checkType (TyB TyVec) (Functor, _)       = pure ()
checkType (TyB TyStream) (Functor, _)    = pure ()
checkType (TyB TyOption) (Functor, _)    = pure ()
checkType (TyB TyStream) (Witherable, _) = pure ()
checkType (TyB TyVec) (Witherable, _)    = pure ()
checkType (TyB TyVec) (Foldable, _)      = pure ()
checkType (TyB TyStream) (Foldable, _)   = pure ()
checkType (TyB TyStr) (IsMonoid, _)      = pure ()
checkType (TyB TyVec:$_) (IsMonoid, _)   = pure ()
checkType (TyB TyStr) (IsPrintf, _)      = pure ()
checkType (TyB TyFloat) (IsPrintf, _)    = pure ()
checkType (TyB TyI) (IsPrintf, _)        = pure ()
checkType (TyB TyBool) (IsPrintf, _)     = pure ()
checkType (TyTup tys) (c@IsPrintf, l)    | not$any nest tys = traverse_ (`checkType` (c, l)) tys
checkType (Rho _ rs) (c@IsPrintf, l)     | tys <- IM.elems rs, not$any nest tys = traverse_ (`checkType` (c, l)) tys
checkType ty (c, l)                      = throwError $ Doesn'tSatisfy l ty c

nest TyTup{}=True; nest Rho{}=True; nest _=False

checkClass :: Ord a
           => IM.IntMap T -- ^ Unification result
           -> Int
           -> S.Set (C, a)
           -> TyM a ()
checkClass tys i cs = {-# SCC "checkClass" #-}
    case substInt tys i of
        Just ty -> traverse_ (checkType ty) (S.toList cs)
        Nothing -> pure ()

lookupVar :: Nm a -> TyM a T
lookupVar n@(Nm _ (U i) l) = do
    st <- gets varEnv
    case IM.lookup i st of
        Just ty -> pure ty -- liftCloneTy ty
        Nothing -> throwError $ IllScoped l n

tyOf :: Ord a => E a -> TyM a T
tyOf = fmap eLoc . tyE

tyDS :: Ord a => Subst -> D a -> TyM a (D T, Subst)
tyDS s (SetFS bs)  = pure (SetFS bs, s)
tyDS s (SetRS bs)  = pure (SetRS bs, s)
tyDS s (SetOFS bs) = pure (SetOFS bs, s)
tyDS s (SetORS bs) = pure (SetORS bs, s)
tyDS s SetAsv      = pure (SetAsv, s)
tyDS s SetUsv      = pure (SetUsv, s)
tyDS s FlushDecl   = pure (FlushDecl, s)
tyDS s (FunDecl n@(Nm _ (U i) _) [] e) = do
    (e', s') <- tyES s e
    let t=eLoc e'
    modify (addVarEnv i t) $> (FunDecl (n$>t) [] e', s')
tyDS _ FunDecl{}   = error "Internal error. Should have been desugared by now."

isAmbiguous :: T -> Bool
isAmbiguous TyVar{}        = True
isAmbiguous (TyArr ty ty') = isAmbiguous ty || isAmbiguous ty'
isAmbiguous (ty:$ty')      = isAmbiguous ty || isAmbiguous ty'
isAmbiguous (TyTup tys)    = any isAmbiguous tys
isAmbiguous TyB{}          = False
isAmbiguous Rho{}          = True

checkAmb :: E T -> TyM a ()
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
checkAmb e@(ParseAllCol t) | isAmbiguous t = throwError (Ambiguous t (void e))
checkAmb e@(ParseCol t _) | isAmbiguous t = throwError (Ambiguous t (void e))
checkAmb _ = pure ()

tS _ s []     = pure ([], s)
tS f s (t:ts) = do {(x, next) <- f s t; first (x:) <$> tS f next ts}

tyP :: Ord a => Program a -> TyM a (Program T)
tyP (Program ds e) = do
    (ds', s0) <- tS tyDS mempty ds
    (e', s1) <- tyES s0 e
    toCheck <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass s1)) toCheck
    let res = {-# SCC "aT" #-} fmap (aT s1) (Program ds' e')
    checkAmb (expr res) $> res

tyNumOp :: Ord a => a -> TyM a T
tyNumOp l = do
    m <- freshN "m"
    modify (mapCV (addC m (IsNum, l)))
    let m' = var m
    pure $ m' ~> m' ~> m'

tySemiOp :: Ord a => a -> TyM a T
tySemiOp l = do
    m <- freshN "m"
    modify (mapCV (addC m (IsSemigroup, l)))
    let m' = var m
    pure $ m' ~> m' ~> m'

tyOrd :: Ord a => a -> TyM a T
tyOrd l = do
    a <- freshN "a"
    modify (mapCV (addC a (IsOrd, l)))
    let a' = var a
    pure $ a' ~> a' ~> tyB

tyEq :: Ord a => a -> TyM a T
tyEq l = do
    a <- freshN "a"
    modify (mapCV (addC a (IsEq, l)))
    let a' = var a
    pure $ a' ~> a' ~> tyB

-- min/max
tyM :: Ord a => a -> TyM a T
tyM l = do
    a <- freshN "a"
    modify (mapCV (addC a (IsOrd, l)))
    let a' = var a
    pure $ a' ~> a' ~> a'

desugar :: a
desugar = error "Should have been de-sugared in an earlier stage!"

tyE :: Ord a => E a -> TyM a (E T)
tyE e = do
    (e', s) <- tyES mempty e
    cvs <- gets (IM.toList . classVars)
    traverse_ (uncurry (checkClass s)) cvs
    pure (fmap (aT s) e')

tyES :: Ord a => Subst -> E a -> TyM a (E T, Subst)
tyES s (Lit _ (BLit b))   = pure (Lit tyB (BLit b), s)
tyES s (Lit _ (ILit i))   = pure (Lit tyI (ILit i), s)
tyES s (Lit _ (FLit f))   = pure (Lit tyF (FLit f), s)
tyES s (Lit _ (StrLit str)) = pure (Lit tyStr (StrLit str), s)
tyES s (RegexLit _ rr)    = pure (RegexLit tyR rr, s)
tyES s (Column _ i)       = pure (Column (tyStream tyStr) i, s)
tyES s (IParseCol _ i)    = pure (IParseCol (tyStream tyI) i, s)
tyES s (FParseCol _ i)    = pure (FParseCol (tyStream tyF) i, s)
tyES s (Field _ i)        = pure (Field tyStr i, s)
tyES s LastField{}        = pure (LastField tyStr, s)
tyES s AllField{}         = pure (AllField tyStr, s)
tyES s FieldList{}        = pure (FieldList (tyV tyStr), s)
tyES s AllColumn{}        = pure (AllColumn (tyStream tyStr), s)
tyES s FParseAllCol{}     = pure (FParseAllCol (tyStream tyF), s)
tyES s IParseAllCol{}     = pure (IParseAllCol (tyStream tyI), s)
tyES s (ParseAllCol l)    = do {a <- freshN "a"; modify (mapCV (addC a (IsParse, l))); pure (ParseAllCol (tyStream (var a)), s)}
tyES s (NB l MZ)    = do {m <- freshN "m"; modify (mapCV (addC m (IsMonoid, l))); pure (NB (var m) MZ, s)}
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
tyES s (BB _ Split) = pure (BB (tyStr ~> tyR ~> tyV tyStr) Split, s)
tyES s (BB _ Splitc) = pure (BB (tyStr ~> tyStr ~> tyV tyStr) Splitc, s)
tyES s (BB _ Matches) = pure (BB (tyStr ~> tyR ~> tyB) Matches, s)
tyES s (BB _ NotMatches) = pure (BB (tyStr ~> tyR ~> tyB) NotMatches, s)
tyES s (BB _ MMatch) = pure (BB (tyStr ~> tyR ~> tyOpt tyStr) MMatch, s)
tyES s (UB _ Tally) = pure (UB (tyStr ~> tyI) Tally, s)
tyES s (BB _ Take) = do {a <- var<$>freshN "a"; pure (BB (tyI ~> tyV a ~> tyV a) Take, s)}
tyES s (BB _ Drop) = do {a <- var<$>freshN "a"; pure (BB (tyI ~> tyV a ~> tyV a) Drop, s)}
tyES s (BB _ Div) = pure (BB (tyF ~> tyF ~> tyF) Div, s)
tyES s (UB _ Not) = pure (UB (tyB ~> tyB) Not, s)
tyES s (BB _ And) = pure (BB (tyB ~> tyB ~> tyB) And, s)
tyES s (BB _ Or) = pure (BB (tyB ~> tyB ~> tyB) Or, s)
tyES s (BB _ Match) = pure (BB (tyStr ~> tyR ~> tyOpt (TyTup [tyI, tyI])) Match, s)
tyES s (TB _ Substr) = pure (TB (tyStr ~> tyI ~> tyI ~> tyStr) Substr, s)
tyES s (TB _ Sub1) = pure (TB (tyR ~> tyStr ~> tyStr ~> tyStr) Sub1, s)
tyES s (TB _ Subs) = pure (TB (tyR ~> tyStr ~> tyStr ~> tyStr) Subs, s)
tyES s (UB _ IParse) = pure (UB (tyStr ~> tyI) IParse, s)
tyES s (UB _ FParse) = pure (UB (tyArr tyStr tyF) FParse, s)
tyES s (UB _ Floor) = pure (UB (tyArr tyF tyI) Floor, s)
tyES s (UB _ Ceiling) = pure (UB (tyF ~> tyI) Ceiling, s)
tyES s (UB _ Head) = do {a <- freshTV "a"; pure (UB (tyV a ~> a) Head, s)}
tyES s (UB _ Tail) = do {a <- freshTV "a"; pure (UB (tyV a ~> tyV a) Tail, s)}
tyES s (UB _ Last) = do {a <- freshTV "a"; pure (UB (tyV a ~> a) Last, s)}
tyES s (UB _ Init) = do {a <- freshTV "a"; pure (UB (tyV a ~> tyV a) Init, s)}
tyES s (BB _ Report) = do {a <- var <$> freshN "a"; b <- var <$> freshN "b"; pure (BB (tyStream a ~> b ~> TyB TyUnit) Report, s)}
tyES s (UB _ TallyList) = do {a <- freshTV "a"; pure (UB (a ~> tyI) TallyList, s)}
tyES s (UB l Negate) = do {a <- freshN "a"; modify (mapCV (addC a (IsNum, l))); let a'=var a in pure (UB (tyArr a' a') Negate, s)}
tyES s (UB _ Some) = do {a <- freshTV "a"; pure (UB (tyArr a (tyOpt a)) Some, s)}
tyES s (NB _ None) = do {a <- freshN "a"; pure (NB (tyOpt (var a)) None, s)}
tyES s (ParseCol l i) = do {a <- freshN "a"; modify (mapCV (addC a (IsParse, l))); pure (ParseCol (tyStream (var a)) i, s)}
tyES s (UB l Parse) = do {a <- freshN "a"; modify (mapCV (addC a (IsParse, l))); pure (UB (tyStr ~> var a) Parse, s)}
tyES s (BB l Sprintf) = do {a <- freshN "a"; modify (mapCV (addC a (IsPrintf, l))); pure (BB (tyStr ~> var a ~> tyStr) Sprintf, s)}
tyES s (BB l DedupOn) = do {a <- var <$> freshN "a"; b <- freshN "b"; modify (mapCV (addC b (IsEq, l))); let b'=var b in pure (BB (tyArr (a ~> b') (tyArr (tyStream a) (tyStream b'))) DedupOn, s)}
tyES s (UB _ (At i)) = do {a <- var <$> freshN "a"; pure (UB (tyV a ~> a) (At i), s)}
tyES s (UB l Dedup) = do {a <- freshN "a"; modify (mapCV (addC a (IsEq, l))); let sA=tyStream (var a) in pure (UB (sA ~> sA) Dedup, s)}
tyES s (UB _ Const) = do {a <- var <$> freshN "a"; b <- var <$> freshN "b"; pure (UB (a ~> b ~> a) Const, s)}
tyES s (UB l CatMaybes) = do {a <- freshN "a"; f <- freshN "f"; modify (mapCV (addC f (Witherable, l))); let a'=var a; f'=var f in pure (UB (tyArr (f':$tyOpt a') (f':$a')) CatMaybes, s)}
tyES s (BB l Filter) = do {a <- freshN "a"; f <- freshN "f"; modify (mapCV (addC f (Witherable, l))); let a'=var a; f'=var f; w=f':$a' in pure (BB ((a' ~> tyB) ~> w ~> w) Filter, s)}
tyES s (UB _ (Select i)) = do
    ρ <- freshN "ρ"; a <- var <$> freshN "a"
    pure (UB (Rho ρ (IM.singleton i a) ~> a) (Select i), s)
tyES s (BB l MapMaybe) = do
    a <- var <$> freshN "a"; b <- var <$> freshN "b"
    f <- freshN "f"
    modify (mapCV (addC f (Witherable, l)))
    let f'=var f
    pure (BB (tyArr (a ~> tyOpt b) ((f':$a) ~> (f':$b))) MapMaybe, s)
tyES s (BB l Map) = do
    a <- var <$> freshN "a"; b <- var <$> freshN "b"
    f <- freshN "f"
    let f'=var f
    modify (mapCV (addC f (Functor, l)))
    pure (BB (tyArr (a ~> b) ((f':$a) ~> (f':$b))) Map, s)
tyES s (TB l Fold) = do
    a <- var <$> freshN "a"; b <- var <$> freshN "b"
    f <- freshN "f"
    let f'=var f
    modify (mapCV (addC f (Foldable, l)))
    pure (TB ((b ~> a ~> b) ~> (b ~> (f':$a) ~> b)) Fold, s)
tyES s (BB l Fold1) = do
    a <- var <$> freshN "a"
    f <- freshN "f"
    let f'=var f
    modify (mapCV (addC f (Foldable, l)))
    pure (BB ((a ~> a ~> a) ~> ((f':$a) ~> a)) Fold1, s)
tyES s (TB _ Bookend) = pure (TB (tyR ~> tyR ~> tyStream tyStr ~> tyStream tyStr) Bookend, s)
tyES s (TB _ Captures) = pure (TB (tyStr ~> tyI ~> tyR ~> tyOpt tyStr) Captures, s)
tyES s (BB _ Prior) = do
    a <- var <$> freshN "a"; b <- var <$> freshN "b"
    pure (BB (tyArr (a ~> a ~> b) (tyStream a ~> tyStream b)) Prior, s)
tyES s (TB _ ZipW) = do
    a <- var <$> freshN "a"; b <- var <$> freshN "b"; c <- var <$> freshN "c"
    pure (TB (tyArr (a ~> b ~> c) (tyStream a ~> tyStream b ~> tyStream c)) ZipW, s)
tyES s (TB _ Scan) = do
    a <- var <$> freshN "a"; b <- var <$> freshN "b"
    pure (TB (tyArr (b ~> a ~> b) (b ~> tyStream a ~> tyStream b)) Scan, s)
tyES s (TB _ Option) = do
    a <- var <$> freshN "a"; b <- var <$> freshN "b"
    pure (TB (b ~> (a ~> b) ~> tyOpt a ~> b) Option, s)
tyES s (TB _ AllCaptures) = pure (TB (tyStr ~> tyI ~> tyR ~> tyV tyStr) AllCaptures, s)
tyES s (Implicit _ e) = do {(e',s') <- tyES s e; pure (Implicit (tyStream (eLoc e')) e', s')}
tyES s (Guarded l e se) = do
    (se', s0) <- tyES s se
    (e', s1) <- tyES s0 e
    s2 <- liftEither $ mguPrep l s1 tyB (eLoc e')
    pure (Guarded (tyStream (eLoc se')) e' se', s2)
tyES s (EApp l e0 e1)     = do
    a <- freshN "a"; b <- freshN "b"
    let a'=var a; b'=var b; e0Ty=a' ~> b'
    (e0', s0) <- tyES s e0
    (e1', s1) <- tyES s0 e1
    s2 <- liftEither $ mguPrep l s1 (eLoc e0') e0Ty
    s3 <- liftEither $ mguPrep l s2 (eLoc e1') a'
    pure (EApp b' e0' e1', s3)
tyES s (Lam _ n@(Nm _ (U i) _) e) = do
    a <- var <$> freshN "a"
    modify (addVarEnv i a)
    (e', s') <- tyES s e
    pure (Lam (a ~> eLoc e') (n$>a) e', s')
tyES s (Let _ (n@(Nm _ (U i) _), eϵ) e) = do
    (eϵ', s0) <- tyES s eϵ
    let bTy=eLoc eϵ'
    modify (addVarEnv i bTy)
    (e', s1) <- tyES s0 e
    pure (Let (eLoc e') (n$>bTy, eϵ') e', s1)
tyES s (Tup _ es) = do {(es', s') <- tS tyES s es; pure (Tup (TyTup (fmap eLoc es')) es', s')}
tyES s (Var _ n) = do {t <- lookupVar n; pure (Var t (n$>t), s)}
tyES s (OptionVal _ (Just e)) = do {(e', s') <- tyES s e; pure (OptionVal (tyOpt (eLoc e')) (Just e'), s')}
tyES s (OptionVal _ Nothing) = do {a <- var <$> freshN "a"; pure (OptionVal (tyOpt a) Nothing, s)}
tyES s (Arr l v) | V.null v = do
    a <- var <$> freshN "a"
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
    (es', s') <- tS (\sϵ e -> do {(e',s0) <- tyES sϵ e; a <- var <$> freshN "a"; s1 <- liftEither $ mguPrep l s0 (tyStream a) (eLoc e'); pure (e', s1)}) s es
    pure (Anchor (TyB TyUnit) es', s')
tyES _ RC{} = error "Regex should not be compiled at this stage."
tyES _ Dfn{} = desugar; tyES _ ResVar{} = desugar; tyES _ Paren{} = desugar
