module Jacinda.Backend.Trans ( eToM ) where

import           Jacinda.AST
import           Jacinda.M

eToM :: E (T K) -> M
eToM (EApp _ (EApp _ (EApp _ (TB (TyArr _ _ (TyArr _ _ (TyArr _ (TyApp _ (TyB _ TyStream) _) _))) Fold) op) seed) stream) = undefined
