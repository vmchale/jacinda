module Ty.Const ( tyStream, tyOpt, tyV
                , tyStr, tyR, tyI, tyF, tyB
                ) where

import           A

-- | argument assumed to have kind 'Star'
tyStream :: T -> T
tyStream = TyApp (TyB TyStream)

tyB, tyI, tyF, tyStr, tyR :: T
tyB=TyB TyBool; tyI=TyB TyInteger; tyF=TyB TyFloat; tyStr=TyB TyStr; tyR=TyB TyR

tyOpt :: T -> T
tyOpt = TyApp (TyB TyOption)

tyV :: T -> T
tyV = TyApp (TyB TyVec)
