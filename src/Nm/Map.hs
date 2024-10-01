{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}

module Nm.Map ( NmMap (..)
              -- , insert
              -- , member
              , intersectionWith
              , elems
              , fromList
              , keys
              , singleton
              , toList
              ) where

import           Control.Arrow  ((&&&))
import           Data.Bifunctor (first)
import qualified Data.IntMap    as IM
import qualified Data.Text      as T
import           Nm
import           U

data NmMap a = NmMap { xx :: IM.IntMap a, context :: IM.IntMap T.Text } 
             deriving (Eq, Functor, Foldable, Traversable)

instance Semigroup (NmMap a) where
    (<>) (NmMap x y) (NmMap x' y') = NmMap (x<>x') (y<>y')

insert :: Nm a -> b -> NmMap b -> NmMap b
insert (Nm n (U i) _) y (NmMap x ctx)= NmMap (IM.insert i y x) (IM.insert i n ctx)

member :: Nm a -> NmMap b -> Bool
member (Nm _ (U i) _) (NmMap x _) = i `IM.member` x

singleton :: Nm a -> b -> NmMap b
singleton (Nm n (U i) _) x = NmMap (IM.singleton i x) (IM.singleton i n)

intersectionWith :: (a -> b -> c) -> NmMap a -> NmMap b -> NmMap c
intersectionWith f (NmMap x0 c0) (NmMap x1 c1) = NmMap (IM.intersectionWith f x0 x1) (IM.intersection c0 c1)

keys :: NmMap a -> [Int]
keys (NmMap x _) = IM.keys x

elems :: NmMap a -> [a]
elems (NmMap x _) = IM.elems x

toList :: NmMap a -> [(T.Text, a)]
toList (NmMap x ns) = map (first (ns IM.!)) (IM.toList x)

fromList :: [(Nm a, b)] -> NmMap b
fromList xs = NmMap { xx = IM.fromList [ (i,x) | (Nm _ (U i) _, x) <- xs ], context = IM.fromList (map ((unU.unique) &&& name) (fst<$>xs)) }
