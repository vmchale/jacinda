module Data.Vector.Ext ( priorM
                       , prior
                       ) where

import qualified Data.Vector as V

priorM :: Monad m => (a -> a -> m a) -> V.Vector a -> m (V.Vector a)
priorM op xs = V.zipWithM op (V.tail xs) xs

prior :: (a -> a -> a) -> V.Vector a -> V.Vector a
prior op xs = V.zipWith op (V.tail xs) xs
