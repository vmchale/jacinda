{-# LANGUAGE OverloadedStrings #-}

module A.E ( M, nN, eta ) where

import           A
import           Control.Monad              ((<=<))
import           Control.Monad.State.Strict (State, state)
import qualified Data.Text                  as T
import           Nm
import           U

type M = State Int

nN :: T.Text -> a -> M (Nm a)
nN n l = state (\i -> (Nm n (U$i+1) l, i+1))

doms :: T -> [T]
doms (TyArr t t') = t:doms t'; doms _ = []

cLam :: E a -> Int
cLam (Lam _ _ e) = 1+cLam e; cLam _ = 0

tuck :: E a -> (E a -> E a, E a)
tuck (Lam l n e) = let (f, e') = tuck e in (Lam l n.f, e'); tuck e = (id, e)

unseam :: [T] -> M (E T -> E T, E T -> E T)
unseam ts = do
    lApps <- traverse (\t -> do {n <- nN "x" t; pure (\e' -> let t' = eLoc e' in Lam (TyArr t t') n e', \e' -> let TyArr _ cod = eLoc e' in EApp cod e' (Var t n))}) ts
    let (ls, eApps) = unzip lApps
    pure (thread ls, thread (reverse eApps))
    where thread = foldr (.) id

mkLam :: [T] -> E T -> M (E T)
mkLam ts e = do
    (lam, app) <- unseam ts
    pure $ lam (app e)

eta = eM <=< eO

eM :: E T -> M (E T)
eM (EApp t ho@(BB _ Map) op)     = EApp t ho <$> eta op
eM (EApp t ho@(BB _ Filter) op)  = EApp t ho <$> eta op
eM (EApp t ho@(BB _ Prior) op)   = EApp t ho <$> eta op
eM (EApp t ho@(BB _ DedupOn) op) = EApp t ho <$> eta op
eM (EApp t ho@(BB _ Fold1) op)   = EApp t ho <$> eta op
eM (EApp t ho@(TB _ Fold) op)    = EApp t ho <$> eta op
eM (EApp t ho@(TB _ Scan) op)    = EApp t ho <$> eta op
eM (EApp t ho@(TB _ ZipW) op)    = EApp t ho <$> eta op
eM (EApp t e0 e1)                = EApp t <$> eM e0 <*> eM e1
eM (Cond t p e0 e1)              = Cond t <$> eM p <*> eM e0 <*> eM e1
eM (OptionVal t e)               = OptionVal t <$> traverse eM e
eM (Implicit t e)                = Implicit t <$> eM e
eM (Lam t n e)                   = Lam t n <$> eM e
eM (Guarded t p e)               = Guarded t <$> eM p <*> eM e
eM (Tup t es)                    = Tup t <$> traverse eM es
eM (Anchor t es)                 = Anchor t <$> traverse eM es
eM (Arr t es)                    = Arr t <$> traverse eM es
eM (Let t (n, e') e)             = do {e'𝜂 <- eM e'; e𝜂 <- eM e; pure (Let t (n, e'𝜂) e𝜂)}
eM e                             = pure e

-- outermost
eO :: E T -> M (E T)
eO e@(Var t@TyArr{} _)    = mkLam (doms t) e
eO e@(UB t _)             = mkLam (doms t) e
eO e@(BB t _)             = mkLam (doms t) e
eO e@(TB t _)             = mkLam (doms t) e
eO e@(EApp t@TyArr{} _ _) = mkLam (doms t) e
eO e@(Lam t@TyArr{} _ _)  = do
    let l = length (doms t)
        (preL, e') = tuck e
    (lam, app) <- unseam (take (l-cLam e) $ doms t)
    pure (lam (preL (app e')))
eO e                      = pure e
