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
            fM (EApp sT (EApp undefined (EApp undefined (TB (TyArr fopT (TyArr sT (TyArr (TyApp (TyB TyStream) xT) sT))) Fold) fop) seed) xs)
        (EApp _ (UB _ CatMaybes) xs) -> do
            -- op | seed (.? xs) -> [option x (x `op`) y] | seed xs
            let TyArr _ (TyArr xTy _)=eLoc op
                xMT=tyOpt xTy
                sTy=eLoc seed
            s <- nN "seed" sTy; x <- nN "x" xMT
            let sE=Var sTy s; xE=Var xMT x
                popT=TyArr xMT sTy; fopT=TyArr sTy popT
                fop=Lam fopT s (Lam popT x (EApp sTy (EApp undefined (EApp undefined (TB (TyArr sTy (TyArr undefined (TyArr xMT sTy))) Option) sE) (EApp undefined op sE)) xE))
            fM (EApp sTy (EApp undefined (EApp undefined (TB (TyArr fopT (TyArr sTy (TyArr (TyApp (TyB TyStream) xMT) sTy))) Fold) fop) seed) xs)
        _ -> pure (EApp t0 (EApp t1 (EApp t2 ho op) seed) stream')
fM (EApp t0 (EApp t1 ho@(BB _ Fold1) op) stream) | TyApp (TyB TyStream) _ <- eLoc stream = do
    stream' <- fM stream
    case stream' of
        (EApp _ (EApp _ (BB _ Filter) p) xs) ->
            fM (In op (Just p) Nothing xs)
        (Guarded t p e) -> do
            let xT=eLoc e
            x <- nN "x" xT
            fM (In op (Just $ Lam (TyArr xT tyB) x p) Nothing (Implicit t e))
        (EApp _ (EApp _ (BB _ Map) f) xs) ->
            fM (In op Nothing (Just f) xs)
        (EApp _ (UB _ CatMaybes) xs) ->
            undefined
        (EApp _ (EApp _ (BB _ MapMaybe) f) xs) ->
            undefined
        _ -> pure (EApp t0 (EApp t1 ho op) stream')
fM (In op mQ mG stream) = do
    stream' <- fM stream
    case stream' of
        (EApp _ (EApp _ (BB _ Map) f) xs) ->
            case mG of
                Nothing -> fM (In op mQ (Just f) xs)
                Just g -> do
                    h <- f `compose` g
                    fM (In op mQ (Just h) xs)
        (EApp _ (EApp _ (BB _ Filter) p) xs) ->
            case mQ of
                Nothing -> fM (In op (Just p) mG xs)
                Just q | TyArr xT _ <- eLoc q -> do
                    x <- nN "x" xT
                    let xE=Var xT x
                    fM (In op (Just $ Lam (TyArr xT tyB) x (EApp tyB p xE `andE` EApp tyB q xE)) mG xs)
        (Guarded t p e) ->
            case mQ of
                Nothing -> do
                    let xT=eLoc e
                    x <- nN "x" xT
                    fM (In op (Just $ Lam (TyArr xT tyB) x p) Nothing (Implicit t e))
                Just q | TyArr xT _ <- eLoc q -> do
                    x <- nN "x" xT
                    let xE=Var xT x
                    fM (In op (Just $ Lam (TyArr xT tyB) x (p `andE` EApp tyB q xE)) mG (Implicit t e))
        (EApp _ (UB _ CatMaybes) xs) ->
            undefined
        (EApp _ (EApp _ (BB _ MapMaybe) f) xs) ->
            undefined
        _ -> pure (In op mQ mG stream')
fM (Tup t es) = Tup t <$> traverse fM es
fM (EApp t e0 e1) = EApp t <$> fM e0 <*> fM e1
fM (Lam t n e) = Lam t n <$> fM e
fM e = pure e

compose :: E T -> E T -> M (E T)
compose f g | TyArr xT yT <- eLoc g, TyArr _ cod <- eLoc f = do
    x <- nN "x" xT
    let xE=Var xT x
    pure $ Lam (TyArr xT cod) x (EApp cod f (EApp yT g xE))

andE :: E T -> E T -> E T
andE x y | tX <- eLoc x, tY <- eLoc y = EApp tyB (EApp (TyArr tY tyB) (BB (TyArr tX (TyArr tY tyB)) Or) x) y
