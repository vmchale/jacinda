module Jacinda.Backend.Const ( mkI, mkF, mkStr ) where

import qualified Data.ByteString  as BS
import qualified Data.Text        as T
import           Jacinda.AST
import           Jacinda.Ty.Const

mkI :: Integer -> E (T K)
mkI = IntLit tyI

mkF :: Double -> E (T K)
mkF = FloatLit tyF

mkStr :: BS.ByteString -> E (T K)
mkStr = StrLit tyStr
