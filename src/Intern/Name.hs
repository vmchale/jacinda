{-# LANGUAGE DeriveFunctor #-}

module Intern.Name ( Name (..)
                   , TyName
                   , eqName
                   ) where

import qualified Data.Text     as T
import           Intern.Unique
import           Prettyprinter (Pretty (pretty))

data Name a = Name { name   :: T.Text
                   , unique :: !Unique
                   , loc    :: a
                   } deriving (Functor)

-- for testing
eqName :: Name a -> Name a -> Bool
eqName (Name n _ _) (Name n' _ _) = n == n'

instance Eq (Name a) where
    (==) (Name _ u _) (Name _ u' _) = u == u'

instance Pretty (Name a) where
    pretty (Name t _ _) = pretty t

instance Ord (Name a) where
    compare (Name _ u _) (Name _ u' _) = compare u u'

type TyName = Name
