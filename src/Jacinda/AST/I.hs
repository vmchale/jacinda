module Jacinda.AST.I ( inline ) where

import           Control.Monad.State.Strict (State, gets, modify, runState)
import           Data.Bifunctor             (second)
import           Data.Foldable              (traverse_)
import qualified Data.IntMap                as IM
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST
import           Jacinda.Rename
import           Jacinda.Ty

data ISt a = ISt { renames :: !Renames
                 , binds   :: IM.IntMap (E a)
                 }

instance HasRenames (ISt a) where
    rename f s = fmap (\x -> s { renames = x }) (f (renames s))

type M a = State (ISt a)

bind :: Nm a -> E a -> ISt a -> ISt a
bind (Nm _ (U u) _) e (ISt r bs) = ISt r (IM.insert u e bs)

runI i = second (max_.renames) . flip runState (ISt (Renames i mempty) mempty)

inline :: Int -> Program (T K) -> (E (T K), Int)
inline i = runI i . iP where iP (Program ds e) = traverse_ iD ds *> iE e

iD :: D (T K) -> M (T K) ()
iD (FunDecl n [] e) = do {eI <- iE e; modify (bind n eI)}
iD SetFS{}          = pure ()
iD FlushDecl{}      = pure ()
iD FunDecl{}        = desugar

desugar = error "Internal error. Should have been de-sugared in an earlier stage!"

iE :: E (T K) -> M (T K) (E (T K))
iE e@NBuiltin{} = pure e; iE e@UBuiltin{} = pure e; iE e@BBuiltin{} = pure e; iE e@TBuiltin{} = pure e
iE e@Column{} = pure e; iE e@ParseCol{} = pure e; iE e@IParseCol{} = pure e; iE e@FParseCol{} = pure e
iE e@Field{} = pure e; iE e@LastField{} = pure e; iE e@AllField{} = pure e; iE e@AllColumn{} = pure e
iE e@IntLit{} = pure e; iE e@FloatLit{} = pure e; iE e@BoolLit{} = pure e; iE e@StrLit{} = pure e
iE e@RegexLit{} = pure e
iE (EApp t e e') = EApp t <$> iE e <*> iE e'
iE (Guarded t p e) = Guarded t <$> iE p <*> iE e
iE (Implicit t e) = Implicit t <$> iE e
iE (Lam t n e) = Lam t n <$> iE e
iE (Tup t es) = Tup t <$> traverse iE es
iE (Arr t es) = Arr t <$> traverse iE es
iE (Anchor t es) = Anchor t <$> traverse iE es
iE (OptionVal t es) = OptionVal t <$> traverse iE es
iE (Cond t p e e') = Cond t <$> iE p <*> iE e <*> iE e'
iE (Let _ (n, e') e) = do
    eI <- iE e'
    modify (bind n eI) *> iE e
iE e@(Var t (Nm _ (U i) _)) = do
    st <- gets binds
    case IM.lookup i st of
        Just e' -> do {er <- rE e'; pure $ fmap (aT (match (eLoc er) t)) er}
        Nothing -> pure e
iE Dfn{} = desugar; iE Paren{} = desugar; iE ResVar{} = desugar
