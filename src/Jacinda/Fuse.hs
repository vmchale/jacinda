{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Fuse ( fuse ) where

import           A
import           A.E
import           Control.Monad.State.Strict (runState)
import           Ty.Const

fuse :: Int -> E T -> (E T, Int)
fuse i = flip runState i.fM

-- fold1: needs a special "form" for fold1-of-map (pick seed through map)
-- also "filter-of-fold1" b/c we need to pick a seed that isn't filtered.

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
            -- FIXME: does this evaluate e? (could be exception)
        (EApp _ (EApp _ (BB _ Map) f) xs) -> do
            let (TyArr xTy yTy) = eLoc f
                (TyArr sTy _) = eLoc op
            s <- nN "seed" sTy; x <- nN "x" xTy
            let sE=Var sTy s; xE=Var xTy x
                popT=TyArr xTy sTy; fopT=TyArr sTy popT
                fop=Lam fopT s (Lam popT x (EApp undefined (EApp undefined op sE) (EApp yTy f xE)))
            fM (EApp sTy (EApp undefined (EApp undefined (TB (TyArr fopT (TyArr sTy (TyArr (TyApp (TyB TyStream) xTy) sTy))) Fold) fop) seed) xs)
        (EApp _ (EApp _ (BB _ MapMaybe) f) xs) -> do
            -- op | seed (f:?xs) -> [option x (x `op`) (f y)] | seed xs
            let TyArr xT yT=eLoc f
                sT=eLoc seed
            s <- nN "seed" sT; x <- nN "x" xT
            let sE=Var sT s; xE=Var xT x
                popT=TyArr xT sT; fopT=TyArr sT popT
                fop=Lam fopT s (Lam popT x (EApp sT (EApp undefined (EApp undefined (TB (TyArr sT (TyArr undefined (TyArr yT sT))) Option) sE) (EApp undefined op sE)) (EApp yT f xE)))
            pure (EApp sT (EApp undefined (EApp undefined (TB (TyArr fopT (TyArr sT (TyArr (TyApp (TyB TyStream) xT) sT))) Fold) fop) seed) xs)
        (EApp _ (UB _ CatMaybes) xs) -> do
            -- op | seed (.? stream') -> [option x (x `op`) y] | seed stream'
            let TyArr _ (TyArr xTy _)=eLoc op
                xMT=tyOpt xTy
                sTy=eLoc seed
            s <- nN "seed" sTy; x <- nN "x" xMT
            let sE=Var sTy s; xE=Var xMT x
                popT=TyArr xMT sTy; fopT=TyArr sTy popT
                fop=Lam fopT s (Lam popT x (EApp sTy (EApp undefined (EApp undefined (TB (TyArr sTy (TyArr undefined (TyArr xMT sTy))) Option) sE) (EApp undefined op sE)) xE))
            pure (EApp sTy (EApp undefined (EApp undefined (TB (TyArr fopT (TyArr sTy (TyArr (TyApp (TyB TyStream) xMT) sTy))) Fold) fop) seed) xs)
        _ -> pure (EApp t0 (EApp t1 (EApp t2 ho op) seed) stream')
fM (Tup t es) = Tup t <$> traverse fM es
fM (EApp t e0 e1) = EApp t <$> fM e0 <*> fM e1
fM (Lam t n e) = Lam t n <$> fM e
fM e = pure e
