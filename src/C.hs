{-# LANGUAGE TupleSections #-}

module C ( secondM ) where

secondM :: Functor m => (b -> m b') -> (a, b) -> m (a, b')
secondM f (x,y) = (x,)<$>f y
