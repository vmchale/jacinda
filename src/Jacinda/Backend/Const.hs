module Jacinda.Backend.Const ( mkI, mkF, mkStr, mkB ) where

import           A
import qualified Data.ByteString as BS
import           Ty.Const

mkI :: Integer -> E T
mkI = Lit tyI.ILit

mkF :: Double -> E T
mkF = Lit tyF.FLit

mkB :: Bool -> E T
mkB = Lit tyB.BLit

mkStr :: BS.ByteString -> E T
mkStr = Lit tyStr.StrLit
