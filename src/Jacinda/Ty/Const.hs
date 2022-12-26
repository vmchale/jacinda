module Jacinda.Ty.Const ( tyStream
                        , tyStr, tyR
                        , tyI
                        , tyF
                        , tyBool
                        , hkt
                        , tyOpt
                        , mkVec
                        ) where

import           Jacinda.AST

-- | argument assumed to have kind 'Star'
tyStream :: T K -> T K
tyStream = TyApp Star (TyB (KArr Star Star) TyStream)

tyBool :: T K
tyBool = TyB Star TyBool

tyI :: T K
tyI = TyB Star TyInteger

tyF :: T K
tyF = TyB Star TyFloat

tyStr :: T K
tyStr = TyB Star TyStr

tyR :: T K
tyR = TyB Star TyR

hkt :: T K -> T K -> T K
hkt = TyApp Star

tyOpt :: T K -> T K
tyOpt = hkt (TyB (KArr Star Star) TyOption)

tyVec :: T K
tyVec = TyB (KArr Star Star) TyVec

mkVec :: T K -> T K
mkVec = hkt tyVec
