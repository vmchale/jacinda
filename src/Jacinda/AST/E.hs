{-# LANGUAGE OverloadedStrings #-}

module Jacinda.AST.E ( eta ) where

import           Control.Monad              ((<=<))
import           Control.Monad.State.Strict (State, get, modify)
import           Data.Functor               (($>))
import qualified Data.Text                  as T
import           Intern.Name
import           Intern.Unique
import           Jacinda.AST

type M = State Int

nN :: T.Text -> a -> M (Nm a)
nN n l = do {i <- get; modify (+1) $> Nm n (U$i+1) l}

doms :: T a -> [T a]
doms (TyArr _ t t') = t:doms t'; doms _ = []

cLam :: E a -> Int
cLam (Lam _ _ e) = 1+cLam e; cLam _ = 0

tuck :: E a -> (E a -> E a, E a)
tuck (Lam l n e) = let (f, e') = tuck e in (Lam l n.f, e'); tuck e = (id, e)

unseam :: [T K] -> M (E (T K) -> E (T K), E (T K) -> E (T K))
unseam ts = do
    lApps <- traverse (\t -> do {n <- nN "x" t; pure (\e' -> let t' = eLoc e' in Lam (TyArr Star t t') n e', \e' -> let TyArr _ _ cod = eLoc e' in EApp cod e' (Var t n))}) ts
    let (ls, eApps) = unzip lApps
    pure (thread ls, thread (reverse eApps))
    where thread = foldr (.) id

mkLam :: [T K] -> E (T K) -> M (E (T K))
mkLam ts e = do
    (lam, app) <- unseam ts
    pure $ lam (app e)

eta = eM <=< eO

eM :: E (T K) -> M (E (T K))
eM (EApp t ho@(BBuiltin _ Map) op)     = EApp t ho <$> eta op
eM (EApp t ho@(BBuiltin _ Filter) op)  = EApp t ho <$> eta op
eM (EApp t ho@(BBuiltin _ Prior) op)   = EApp t ho <$> eta op
eM (EApp t ho@(BBuiltin _ DedupOn) op) = EApp t ho <$> eta op
eM (EApp t ho@(BBuiltin _ Fold1) op)   = EApp t ho <$> eta op
eM (EApp t ho@(TBuiltin _ Fold) op)    = EApp t ho <$> eta op
eM (EApp t ho@(TBuiltin _ Scan) op)    = EApp t ho <$> eta op
eM (EApp t ho@(TBuiltin _ ZipW) op)    = EApp t ho <$> eta op
eM (EApp t e0 e1)                      = EApp t <$> eM e0 <*> eM e1
eM (Cond t p e0 e1)                    = Cond t <$> eM p <*> eM e0 <*> eM e1
eM (OptionVal t e)                     = OptionVal t <$> traverse eM e
eM (Implicit t e)                      = Implicit t <$> eM e
eM (Lam t n e)                         = Lam t n <$> eM e
eM (Guarded t p e)                     = Guarded t <$> eM p <*> eM e
eM (Tup t es)                          = Tup t <$> traverse eM es
eM (Anchor t es)                       = Anchor t <$> traverse eM es
eM (Arr t es)                          = Arr t <$> traverse eM es
eM (Let t (n, e') e)                   = do {e'𝜂 <- eM e'; e𝜂 <- eM e; pure (Let t (n, e'𝜂) e𝜂)}
eM e                                   = pure e

-- outermost
eO :: E (T K) -> M (E (T K))
eO e@(Var t@TyArr{} _)    = mkLam (doms t) e
eO e@(UBuiltin t _)       = mkLam (doms t) e
eO e@(BBuiltin t _)       = mkLam (doms t) e
eO e@(TBuiltin t _)       = mkLam (doms t) e
eO e@(EApp t@TyArr{} _ _) = mkLam (doms t) e
eO e@(Lam t@TyArr{} _ _)  = do
    let l = length (doms t)
        (preL, e') = tuck e
    (lam, app) <- unseam (take (l-cLam e) $ doms t)
    pure (lam (preL (app e')))
eO e                      = pure e
