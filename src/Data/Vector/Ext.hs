module Data.Vector.Ext ( priorM_
                       ) where

import qualified Data.Vector as V

priorM_ :: Monad m => (a -> a -> m b) -> V.Vector a -> m ()
priorM_ op xs = V.zipWithM_ op (V.tail xs) xs
