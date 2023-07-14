module Jacinda.Backend.Const ( mkI, mkF, mkStr, mkB ) where

import           A
import qualified Data.ByteString as BS
import           Ty.Const

mkI :: Integer -> E T
mkI = ILit tyI

mkF :: Double -> E T
mkF = FLit tyF

mkB :: Bool -> E T
mkB = BLit tyB

mkStr :: BS.ByteString -> E T
mkStr = StrLit tyStr
