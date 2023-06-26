module Jacinda.Backend.Const ( mkI, mkF, mkStr, mkB, mkJ ) where

import qualified Data.ByteString  as BS
import           Jacinda.AST
import           Jacinda.Ty.Const

mkI :: Integer -> E (T K)
mkI = IntLit tyI

mkF :: Double -> E (T K)
mkF = FloatLit tyF

mkB :: Bool -> E (T K)
mkB = BoolLit tyB

mkStr :: BS.ByteString -> E (T K)
mkStr = StrLit tyStr

mkJ :: E (T K) -> E (T K)
mkJ e = OptionVal (TyApp Star (TyB (KArr Star Star) TyOption) (eLoc e)) (Just e)
