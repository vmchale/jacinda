module Jacinda.Ty.Const ( tyStream
                        , tyStr
                        , tyI
                        , tyF
                        , tyBool
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
