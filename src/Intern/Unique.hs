module Intern.Unique ( Unique (..)
                     ) where

newtype Unique = Unique { unUnique :: Int }
    deriving (Eq, Ord)
