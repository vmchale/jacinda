{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Rename ( rE
                      , renameProgram
                      , runRenameM
                      , renamePGlobal
                      , RenameM
                      , Renames (..)
                      , HasRenames (..)
                      ) where

import           Control.Monad.State.Strict (MonadState, State, runState)
import           Control.Recursion          (cata, embed)
import           Data.Bifunctor             (second)
import qualified Data.IntMap                as IM
import qualified Data.Text                  as T
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST
import           Lens.Micro                 (Lens', over)
import           Lens.Micro.Mtl             (modifying, use, (%=), (.=))

data Renames = Renames { max_ :: Int, bound :: IM.IntMap Int }

class HasRenames a where
    rename :: Lens' a Renames

instance HasRenames Renames where
    rename = id

boundLens :: Lens' Renames (IM.IntMap Int)
boundLens f s = fmap (\x -> s { bound = x }) (f (bound s))

maxLens :: Lens' Renames Int
maxLens f s = fmap (\x -> s { max_ = x }) (f (max_ s))

type RenameM = State Renames

renamePGlobal :: Int -> Program a -> (Program a, Int)
renamePGlobal i = runRenameM i . renameProgram

runRenameM :: Int -> RenameM x -> (x, Int)
runRenameM i act = second max_ (runState act (Renames i IM.empty))

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
    st <- use (rename.maxLens)
    Nm n (U$st+1) l
        <$ modifying (rename.maxLens) (+1)

withRenames :: (HasRenames s, MonadState s m) => (Renames -> Renames) -> m a -> m a
withRenames modSt act = do
    preSt <- use rename
    rename %= modSt
    res <- act
    postMax <- use (rename.maxLens)
    rename .= setMax postMax preSt
    pure res

withName :: (HasRenames s, MonadState s m) => Nm a -> m (Nm a, Renames -> Renames)
withName (Nm t (U i) l) = do
    m <- use (rename.maxLens)
    let newUniq = m+1
    rename.maxLens .= newUniq
    pure (Nm t (U newUniq) l, mapBound (IM.insert i (m+1)))

mapBound :: (IM.IntMap Int -> IM.IntMap Int) -> Renames -> Renames
mapBound f (Renames m b) = Renames m (f b)

setMax :: Int -> Renames -> Renames
setMax i (Renames _ b) = Renames i b

-- | Desguar top-level functions as lambdas
mkLam :: [Nm a] -> E a -> E a
mkLam ns e = foldr (\n -> Lam (loc n) n) e ns

hasY :: E a -> Bool
hasY = cata a where
    a (ResVarF _ Y)           = True
    a (TupF _ es)             = or es
    a (EAppF _ e e')          = e || e'
    a (LamF _ _ e)            = e
    a DfnF{}                  = error "Not supported yet."
    a (LetF _ b e)            = e || snd b
    a (GuardedF _ p b)        = b || p
    a (ImplicitF _ e)         = e
    a (ParenF _ e)            = e
    a (ArrF _ es)             = or es
    a (AnchorF _ es)          = or es
    a (OptionValF _ (Just e)) = e
    a (CondF _ p e e')        = p || e || e'
    a _                       = False

replaceXY :: (a -> Nm a) -- ^ @x@
          -> (a -> Nm a) -- ^ @y@
          -> E a
          -> E a
replaceXY nX nY = cata a where
    a (ResVarF l X) = Var l (nX l)
    a (ResVarF l Y) = Var l (nY l)
    a x             = embed x

replaceX :: (a -> Nm a) -> E a -> E a
replaceX n = cata a where
    a (ResVarF l X) = Var l (n l)
    a x             = embed x

renameD :: D a -> RenameM (D a)
renameD (FunDecl n ns e) = FunDecl n [] <$> rE (mkLam ns e)
renameD d                = pure d

renameProgram :: Program a -> RenameM (Program a)
renameProgram (Program ds e) = Program <$> traverse renameD ds <*> rE e

{-# INLINABLE rE #-}
rE :: (HasRenames s, MonadState s m) => E a -> m (E a)
rE (EApp l e e')   = EApp l <$> rE e <*> rE e'
rE (Tup l es)      = Tup l <$> traverse rE es
rE (Var l n)       = Var l <$> replaceVar n
rE (Lam l n e)     = do
    (n', modR) <- withName n
    Lam l n' <$> withRenames modR (rE e)
rE (Dfn l e) | {-# SCC "hasY" #-} hasY e = do
    x@(Nm nX uX _) <- dummyName l "x"
    y@(Nm nY uY _) <- dummyName l "y"
    Lam l x . Lam l y <$> rE ({-# SCC "replaceXY" #-} replaceXY (Nm nX uX) (Nm nY uY) e)
                  | otherwise = do
    x@(Nm n u _) <- dummyName l "x"
    Lam l x <$> rE ({-# SCC "replaceX" #-} replaceX (Nm n u) e)
rE (Guarded l p e) = Guarded l <$> rE p <*> rE e
rE (Implicit l e) = Implicit l <$> rE e
rE ResVar{} = error "Bare reserved variable."
rE (Let l (n, eϵ) e') = do
    eϵ' <- rE eϵ
    (n', modR) <- withName n
    Let l (n', eϵ') <$> withRenames modR (rE e')
rE (Paren _ e) = rE e
rE (Arr l es) = Arr l <$> traverse rE es
rE (Anchor l es) = Anchor l <$> traverse rE es
rE (OptionVal l e) = OptionVal l <$> traverse rE e
rE (Cond l p e e') = Cond l <$> rE p <*> rE e <*> rE e'
rE e = pure e
