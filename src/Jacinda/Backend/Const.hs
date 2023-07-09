module Jacinda.Backend.Const ( mkI, mkF, mkStr, mkB ) where

import           A
import qualified Data.ByteString as BS
import           Ty.Const

mkI :: Integer -> E (T K)
mkI = ILit tyI

mkF :: Double -> E (T K)
mkF = FLit tyF

mkB :: Bool -> E (T K)
mkB = BLit tyB

mkStr :: BS.ByteString -> E (T K)
mkStr = StrLit tyStr
