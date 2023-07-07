module Jacinda.Backend.Const ( mkI, mkF, mkStr, mkB ) where

import           A
import qualified Data.ByteString as BS
import           Ty.Const

mkI :: Integer -> E (T K)
mkI = IntLit tyI

mkF :: Double -> E (T K)
mkF = FloatLit tyF

mkB :: Bool -> E (T K)
mkB = BoolLit tyB

mkStr :: BS.ByteString -> E (T K)
mkStr = StrLit tyStr
