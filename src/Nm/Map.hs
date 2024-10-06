{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Nm.Map ( NmMap (..)
              , intersectionWith
              , isSubmapOf
              , (!)
              , elems
              , singleton
              , toList
              , fromList
              ) where

import           Control.Arrow  ((&&&))
import           Data.Bifunctor (first)
import qualified Data.IntMap    as IM
import qualified Data.Text      as T
import           Nm
import           U

infixl 9 !

data NmMap a = NmMap { xx :: !(IM.IntMap a), context :: IM.IntMap T.Text }
             deriving (Eq, Functor, Foldable, Traversable)

instance Semigroup (NmMap a) where
    (<>) (NmMap x y) (NmMap x' y') = NmMap (x<>x') (y<>y')

singleton :: Nm a -> b -> NmMap b
singleton (Nm n (U i) _) x = NmMap (IM.singleton i x) (IM.singleton i n)

(!) :: NmMap a -> Nm b -> a
(!) (NmMap x _) (Nm _ (U i) _) = x IM.! i

intersectionWith :: (a -> b -> c) -> NmMap a -> NmMap b -> NmMap c
intersectionWith f (NmMap x0 c0) (NmMap x1 c1) = NmMap (IM.intersectionWith f x0 x1) (IM.intersection c0 c1)

isSubmapOf :: NmMap a -> NmMap b -> Bool
isSubmapOf (NmMap x _) (NmMap y _) = IM.isSubmapOfBy (\_ _ -> True) x y

elems :: NmMap a -> [a]
elems (NmMap x _) = IM.elems x

toList :: NmMap a -> [(Nm (), a)]
toList (NmMap x ns) = map (first (\i -> Nm (ns IM.! i) (U i) ())) (IM.toList x)

fromList :: [(Nm a, b)] -> NmMap b
fromList xs = NmMap { xx = IM.fromList [ (i,x) | (Nm _ (U i) _, x) <- xs ], context = IM.fromList (map ((unU.unique) &&& name) (fst<$>xs)) }
