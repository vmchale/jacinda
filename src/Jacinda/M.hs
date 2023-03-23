module Jacinda.M ( M (..)
                 ) where

import qualified Data.ByteString as BS
import qualified Data.IntMap     as IM
import           Jacinda.AST

type S = IM.IntMap (E (T K))

data M = Step (BS.ByteString -> S -> S) | Do (BS.ByteString -> S -> IO ())
