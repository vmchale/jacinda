module Jacinda.M ( M (..)
                 ) where

import           Jacinda.AST

data M = FoldWither { opW :: E (T K), pW :: E (T K) }
