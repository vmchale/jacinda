{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Fuse ( fuse ) where

import           A
import           A.E
import           Control.Monad.State.Strict (runState)
import           Ty.Const

fuse :: Int -> E T -> (E T, Int)
fuse i = flip runState i.fM

fM :: E T -> M (E T)
fM (EApp t0 (EApp t1 (EApp t2 ho@(TB _ Fold) op) seed) stream) | TyApp (TyB TyStream) _ <- eLoc stream = do
    stream' <- fM stream
    case stream' of
        (EApp _ (EApp _ (BB _ Filter) p) xs) -> do
            let opTy@(TyArr sTy popTy@(TyArr xTy _)) = eLoc op
            s <- nN "seed" sTy; x <- nN "x" xTy
            let sE=Var sTy s; xE=Var xTy x
            let fop=Lam opTy s (Lam popTy x (Cond sTy (EApp tyB p xE) (EApp sTy (EApp popTy op sE) xE) sE)) in pure (EApp t0 (EApp t1 (EApp t2 ho fop) seed) xs)
        (Guarded t p e) -> do
            let opTy@(TyArr sTy popTy@(TyArr xTy _)) = eLoc op
            s <- nN "seed" sTy; x <- nN "x" xTy
            let sE=Var sTy s; xE=Var xTy x
            let fop=Lam opTy s (Lam popTy x (Cond sTy p (EApp sTy (EApp popTy op sE) xE) sE)) in pure (EApp t0 (EApp t1 (EApp t2 ho fop) seed) (Implicit t e))
            -- FIXME: evaluates e (could be exception)
        (EApp _ (EApp _ (BB _ Map) f) xs) -> do
            let (TyArr xTy yTy) = eLoc f
                (TyArr sTy _) = eLoc op
            s <- nN "seed" sTy; x <- nN "x" xTy
            let sE=Var sTy s; xE=Var xTy x
                popT=TyArr xTy sTy; fopT=TyArr sTy popT
                fop=Lam fopT s (Lam popT x (EApp undefined (EApp undefined op sE) (EApp yTy f xE)))
            fM (EApp sTy (EApp undefined (EApp undefined (TB (TyArr fopT (TyArr sTy (TyArr (TyApp (TyB TyStream) xTy) sTy))) Fold) fop) seed) xs)
        _ -> pure (EApp t0 (EApp t1 (EApp t2 ho op) seed) stream')
fM (EApp t0 (EApp t1 ho@(BB _ Fold1) op) stream) | TyApp (TyB TyStream) _ <- eLoc stream = do
    stream' <- fM stream
    case stream' of
        (EApp _ (EApp _ (BB _ Filter) p) xs) -> do
            let opT@(TyArr xT popT) = eLoc op
            s <- nN "s" xT; x <- nN "x" xT
            let sE=Var xT s; xE=Var xT x
                fop=Lam opT s (Lam popT x (Cond xT (EApp tyB p xE) (EApp xT (EApp popT op sE) xE) sE))
            pure (EApp t0 (EApp t1 ho fop) xs)
        (Guarded t p e) -> do
            let opT@(TyArr xT popT) = eLoc op
            s <- nN "s" xT; x <- nN "x" xT
            let sE=Var xT s; xE=Var xT x
                fop=Lam opT s (Lam popT x (Cond xT p (EApp xT (EApp popT op sE) xE) sE))
            pure (EApp t0 (EApp t1 ho fop) (Implicit t e))
        _ -> pure (EApp t0 (EApp t1 ho op) stream')
fM (Tup t es) = Tup t <$> traverse fM es
fM (EApp t e0 e1) = EApp t <$> fM e0 <*> fM e1
fM (Lam t n e) = Lam t n <$> fM e
fM e = pure e
