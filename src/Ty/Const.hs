module Ty.Const ( tyStream
                , tyStr, tyR
                , tyI
                , tyF
                , tyB
                , hkt
                , tyOpt
                , tyV
                ) where

import           A

-- | argument assumed to have kind 'Star'
tyStream :: T -> T
tyStream = TyApp (TyB TyStream)

tyB :: T
tyB = TyB TyBool

tyI :: T
tyI = TyB TyInteger

tyF :: T
tyF = TyB TyFloat

tyStr :: T
tyStr = TyB TyStr

tyR :: T
tyR = TyB TyR

hkt :: T -> T -> T
hkt = TyApp

tyOpt :: T -> T
tyOpt = hkt (TyB TyOption)

tyVec :: T
tyVec = TyB TyVec

tyV :: T -> T
tyV = TyApp tyVec
