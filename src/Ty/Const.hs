module Ty.Const ( tyStream, tyOpt, tyV
                , tyStr, tyR, tyI, tyF, tyB
                ) where

import           A

-- | argument assumed to have kind 'Star'
tyStream :: T -> T
tyStream = (TyB TyStream:$)

tyB, tyI, tyF, tyStr, tyR :: T
tyB=TyB TyBool; tyI=TyB TyI; tyF=TyB TyFloat; tyStr=TyB TyStr; tyR=TyB TyR

tyOpt :: T -> T
tyOpt = (TyB TyOption:$)

tyV :: T -> T
tyV = (TyB TyVec:$)
