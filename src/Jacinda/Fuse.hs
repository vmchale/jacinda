{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Fuse ( fuse ) where

import           Jacinda.AST
import           Jacinda.AST.E

fuse :: E (T K) -> M (E (T K))
fuse (EApp t0 (EApp t1 (EApp t2 f@(TB (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyStream) _) _))) Fold) op) seed) stream) = do
    stream' <- fuse stream
    case stream' of
        (EApp _ (EApp _ (BB _ Filter) p) xs) -> do
            let opTy@(TyArr _ sTy popTy@(TyArr _ xTy _))= eLoc op
            s <- nN "seed" sTy; x <- nN "x" xTy
            let fop=Lam opTy s (Lam popTy x undefined) in pure (EApp t0 (EApp t1 (EApp t2 f fop) seed) xs)
        _ -> pure (EApp t0 (EApp t1 (EApp t2 f op) seed) stream')
