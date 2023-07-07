{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Fuse ( fuse ) where

import           A
import           A.E
import           Control.Monad.State.Strict (runState)
import           Ty.Const                   (tyStream)

fuse :: Int -> E (T K) -> (E (T K), Int)
fuse i = flip runState i.fM

fM :: E (T K) -> M (E (T K))
fM (EApp t0 (EApp t1 (EApp t2 ho@(TB (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyStream) _) _))) Fold) op) seed) stream) = do
    stream' <- fM stream
    case stream' of
        (EApp _ (EApp _ (BB _ Filter) p) xs) -> do
            let opTy@(TyArr _ sTy popTy@(TyArr _ xTy _)) = eLoc op
            s <- nN "seed" sTy; x <- nN "x" xTy
            let sE=Var sTy s; xE=Var xTy x
            let fop=Lam opTy s (Lam popTy x (Cond sTy p (EApp sTy (EApp popTy op sE) xE) sE)) in pure (EApp t0 (EApp t1 (EApp t2 ho fop) seed) xs)
        (EApp _ (EApp _ (BB _ Map) f) xs) -> do
            let (TyArr _ xTy yTy) = eLoc f
                (TyArr _ sTy _) = eLoc op
            s <- nN "seed" sTy; x <- nN "x" xTy
            let sE=Var sTy s; xE=Var xTy x
                popT=TyArr Star xTy sTy; fopT=TyArr Star sTy popT
                fop=Lam fopT s (Lam popT x (EApp undefined (EApp undefined op sE) (EApp yTy f xE)))
            fM (EApp sTy (EApp undefined (EApp undefined (TB (TyArr Star fopT (TyArr Star sTy (TyArr Star (TyApp Star (TyB undefined TyStream) xTy) sTy))) Fold) fop) seed) xs)
        _ -> pure (EApp t0 (EApp t1 (EApp t2 ho op) seed) stream')
fM (Tup t es) = Tup t <$> traverse fM es
fM (EApp t e0 e1) = EApp t <$> fM e0 <*> fM e1
fM (Lam t n e) = Lam t n <$> fM e
fM e = pure e
