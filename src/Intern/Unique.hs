module Intern.Unique ( U (..)
                     ) where

newtype U = U { unU :: Int }
    deriving (Eq, Ord)
