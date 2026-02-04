module R ( rE, rP
         , Renames (..)
         , HasRenames (..)
         ) where

import           A
import           C
import           Control.Monad.State.Strict (MonadState, State, runState)
import           Data.Bifunctor             (bimap, first, second)
import qualified Data.IntMap                as IM
import qualified Data.Text                  as T
import qualified Data.Vector                as V
import           Lens.Micro                 (Lens', over)
import           Lens.Micro.Mtl             (use, (%=), (.=))
import           Nm
import           U

data Renames = Rs { max_ :: Int, bound :: IM.IntMap Int }

class HasRenames a where
    rename :: Lens' a Renames

instance HasRenames Renames where rename=id

boundLens :: Lens' Renames (IM.IntMap Int)
boundLens f (Rs m b) = Rs m <$> f b

maxLens :: Lens' Renames Int
maxLens f (Rs m b) = (\x -> Rs x b) <$> f m

type RenameM = State Renames

rP :: Int -> Program a -> (Program a, Int)
rP i = runRM i.renameProgram

runRM :: Int -> RenameM x -> (x, Int)
runRM i act = second max_ (runState act (Rs i IM.empty))

replaceUnique :: (MonadState s m, HasRenames s) => U -> m U
replaceUnique u@(U i) = do
    rSt <- use (rename.boundLens)
    case IM.lookup i rSt of
        Nothing -> pure u
        Just j  -> withRenames (over boundLens (IM.delete i)) $ replaceUnique (U j)

replaceVar :: (MonadState s m, HasRenames s) => Nm a -> m (Nm a)
replaceVar (Nm n u l) = do
    u' <- replaceUnique u
    pure $ Nm n u' l

dummyName :: (MonadState s m, HasRenames s) => a -> T.Text -> m (Nm a)
dummyName l n = do
    rename.maxLens %= (+1)
    st <- use (rename.maxLens)
    pure $ Nm n (U st) l

doLocal :: (HasRenames s, MonadState s m) => m a -> m a
doLocal act = do
    preB <- use (rename.boundLens)
    act <* (rename.boundLens .= preB)

freshen :: (HasRenames s, MonadState s m) => Nm a -> m (Nm a)
freshen (Nm n (U i) l) = do
    rename.maxLens %= (+1)
    nU <- use (rename.maxLens)
    rename.boundLens %= IM.insert i nU
    pure (Nm n (U nU) l)

withRenames :: (HasRenames s, MonadState s m) => (Renames -> Renames) -> m a -> m a
withRenames modSt act = do
    preSt <- use rename
    rename %= modSt
    res <- act
    postMax <- use (rename.maxLens)
    rename .= setMax postMax preSt
    pure res

setMax :: Int -> Renames -> Renames
setMax i (Rs _ b) = Rs i b

-- | Desguar top-level functions as lambdas
mkLam :: [Nm a] -> E a -> E a
mkLam ns e = foldr (\n -> Lam (loc n) n) e ns

renameD :: D a -> RenameM (D a)
renameD (FunDecl l n ns e) = FunDecl l n [] <$> rE (mkLam ns e)
renameD d                  = pure d

renameProgram :: Program a -> RenameM (Program a)
renameProgram (Program ds e) = Program <$> traverse renameD ds <*> rE e

{-# INLINABLE rE #-}
rE :: (HasRenames s, MonadState s m) => E a -> m (E a)
rE = fmap fst.r undefined undefined

{-# INLINABLE r #-}
r :: (HasRenames s, MonadState s m) => (a -> Nm a) -> (a -> Nm a) -> E a -> m (E a, Bool)
r _ _ (Var l n)      = (\n' -> (Var l n', False)) <$> replaceVar n
r x y (EApp l e0 e1) = do
    (e0',b0) <- r x y e0
    (e1',b1) <- r x y e1
    pure (EApp l e0' e1', b0||b1)
r x y (Cond l p e0 e1) = do
    (p',b0) <- r x y p
    (e0',b1) <- r x y e0
    (e1',b2) <- r x y e1
    pure (Cond l p' e0' e1', b0||b1||b2)
r x y (Lam l n e)   = doLocal $ do
    n' <- freshen n
    first (Lam l n') <$> r x y e
r x y (Let l (n, eb) e) = doLocal $ do
    (eb', b) <- r x y eb
    n' <- freshen n
    bimap (Let l (n', eb')) (b||) <$> r x y e
r _ _ (Dfn l e) = do
    x@(Nm nX uX _) <- dummyName l "x"
    y@(Nm nY uY _) <- dummyName l "y"
    (e',hasY) <- r (Nm nX uX) (Nm nY uY) e
    pure $ if hasY
        then (Lam l x (Lam l y e'), False)
        else (Lam l x e', False)
r x y (Implicit l e) = do
    (e',b) <- r x y e
    pure (Implicit l e', b)
r x y (Guarded l p e) = do
    (p',b0) <- r x y p
    (e',b1) <- r x y e
    pure (Guarded l p' e', b0||b1)
r x _ (ResVar l X)   = pure (Var l (x l), False)
r _ y (ResVar l Y)   = pure (Var l (y l), True)
r x y (Tup l es) = do
    (es',b) <- unzip <$> traverse (r x y) es
    pure (Tup l es', or b)
r x y (OptionVal l e) = do
    v <- traverse (r x y) e
    case v of
        Nothing     -> pure (OptionVal l Nothing, False)
        Just (e',b) -> pure (OptionVal l (Just e'), b)
r x y (Rec l es) = do
    (es',b) <- unzip . map (\(n,(eϵ,b)) -> ((n,eϵ),b)) <$> traverse (secondM (r x y)) es
    pure (Rec l es', or b)
r x y (Anchor l es) = do
    (es',b) <- unzip <$> traverse (r x y) es
    pure (Anchor l es', or b)
r x y (Arr l es) = do
    (es',b) <- V.unzip <$> traverse (r x y) es
    pure (Arr l es', or b)
r x y (Paren _ e') = r x y e'
r _ _ e' = pure (e', False)
