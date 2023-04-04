module Jacinda.Backend.P ( runJac ) where

import qualified Data.ByteString        as BS
import           Jacinda.AST
import           Jacinda.AST.I
import           Jacinda.Backend.Stream
import           Regex.Rure             (RurePtr)

runJac :: RurePtr -- ^ Record separator
       -> Int
       -> Program (T K)
       -> Either StreamError ([BS.ByteString] -> IO ())
runJac re i e = fileProcessor re (flushD e) (fst$ib i e)
