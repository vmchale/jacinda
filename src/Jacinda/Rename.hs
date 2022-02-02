{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Rename ( renameE
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
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying, use, (%=), (.=))

data Renames = Renames { max_ :: Int, bound :: IM.IntMap Int }

-- TODO: instance Pretty Renames for debug?

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

-- Make sure you don't have cycles in the renames map!
replaceUnique :: (MonadState s m, HasRenames s) => Unique -> m Unique
replaceUnique u@(Unique i) = do
    rSt <- use (rename.boundLens)
    case IM.lookup i rSt of
        Nothing -> pure u
        Just j  -> replaceUnique (Unique j)

replaceVar :: (MonadState s m, HasRenames s) => Name a -> m (Name a)
replaceVar (Name n u l) = do
    u' <- replaceUnique u
    pure $ Name n u' l

dummyName :: (MonadState s m, HasRenames s) => a -> T.Text -> m (Name a)
dummyName l n = do
    st <- use (rename.maxLens)
    Name n (Unique $ st+1) l
        <$ modifying (rename.maxLens) (+1)

-- allows us to work with a temporary change to the renamer state, tracking the
-- max sensibly
withRenames :: (HasRenames s, MonadState s m) => (Renames -> Renames) -> m a -> m a
withRenames modSt act = do
    preSt <- use rename
    rename %= modSt
    res <- act
    postMax <- use (rename.maxLens)
    rename .= setMax postMax preSt
    pure res

withName :: (HasRenames s, MonadState s m) => Name a -> m (Name a, Renames -> Renames)
withName (Name t (Unique i) l) = do
    m <- use (rename.maxLens)
    let newUniq = m+1
    rename.maxLens .= newUniq
    pure (Name t (Unique newUniq) l, mapBound (IM.insert i (m+1)))

mapBound :: (IM.IntMap Int -> IM.IntMap Int) -> Renames -> Renames
mapBound f (Renames m b) = Renames m (f b)

setMax :: Int -> Renames -> Renames
setMax i (Renames _ b) = Renames i b

-- | Desguar top-level functions as lambdas
mkLam :: [Name a] -> E a -> E a
mkLam ns e = foldr (\n -> Lam (loc n) n) e ns

-- TODO: investigate performance w/out cata

-- | A dfn could be unary or binary - here we guess if it is binary
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

replaceXY :: (a -> Name a) -- ^ @x@
          -> (a -> Name a) -- ^ @y@
          -> E a
          -> E a
replaceXY nX nY = cata a where
    a (ResVarF l X) = Var l (nX l)
    a (ResVarF l Y) = Var l (nY l)
    a x             = embed x

replaceX :: (a -> Name a) -> E a -> E a
replaceX n = cata a where
    a (ResVarF l X) = Var l (n l)
    a x             = embed x

renameD :: D a -> RenameM (D a)
renameD d@SetFS{}        = pure d
renameD (FunDecl n ns e) = FunDecl n [] <$> renameE (mkLam ns e)

renameProgram :: Program a -> RenameM (Program a)
renameProgram (Program ds e) = Program <$> traverse renameD ds <*> renameE e

renameE :: (HasRenames s, MonadState s m) => E a -> m (E a)
renameE (EApp l e e')   = EApp l <$> renameE e <*> renameE e'
renameE (Tup l es)      = Tup l <$> traverse renameE es
renameE (Var l n)       = Var l <$> replaceVar n
renameE (Lam l n e)     = do
    (n', modR) <- withName n
    Lam l n' <$> withRenames modR (renameE e)
renameE (Dfn l e) | {-# SCC "hasY" #-} hasY e = do
    x@(Name nX uX _) <- dummyName l "x"
    y@(Name nY uY _) <- dummyName l "y"
    Lam l x . Lam l y <$> renameE ({-# SCC "replaceXY" #-} replaceXY (Name nX uX) (Name nY uY) e)
                  | otherwise = do
    x@(Name n u _) <- dummyName l "x"
    -- no need for withName... withRenames because this is fresh/globally unique
    Lam l x <$> renameE ({-# SCC "replaceX" #-} replaceX (Name n u) e)
renameE (Guarded l p e) = Guarded l <$> renameE p <*> renameE e
renameE (Implicit l e) = Implicit l <$> renameE e
renameE ResVar{} = error "Bare reserved variable."
renameE (Let l (n, eϵ) e') = do
    eϵ' <- renameE eϵ
    (n', modR) <- withName n
    Let l (n', eϵ') <$> withRenames modR (renameE e')
renameE (Paren _ e) = renameE e
renameE (Arr l es) = Arr l <$> traverse renameE es
renameE (Anchor l es) = Anchor l <$> traverse renameE es
renameE (OptionVal l e) = OptionVal l <$> traverse renameE e
renameE (Cond l p e e') = Cond l <$> renameE p <*> renameE e <*> renameE e'
renameE e = pure e -- literals &c.
