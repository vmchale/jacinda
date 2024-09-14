module Ty.Const ( tyStream, tyOpt, tyV
                , tyStr, tyR, tyI, tyF, tyB
                ) where

import           A

tyB, tyI, tyF, tyStr, tyR :: T
tyB=TyB TyBool; tyI=TyB TyI; tyF=TyB TyFloat; tyStr=TyB TyStr; tyR=TyB TyR

-- | argument assumed to have kind 'Star'
tyOpt, tyV, tyStream :: T -> T
tyOpt = (TyB TyOption:$); tyStream = (TyB TyStream:$); tyV = (TyB TyVec:$)
