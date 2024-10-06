module Data.Vector.Ext (scanlM') where

import           Data.Vector.Fusion.Bundle.Monadic (fromVector)
import qualified Data.Vector.Fusion.Bundle.Monadic as Bundle
import           Data.Vector.Generic               (Vector, unstreamM)

scanlM' :: (Monad m, Vector v a, Vector v b) => (a -> b -> m a) -> a -> v b -> m (v a)
scanlM' op seed = unstreamM . Bundle.scanlM' op seed . fromVector
-- scanlM'
