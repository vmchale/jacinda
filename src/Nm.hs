{-# LANGUAGE DeriveFunctor #-}

module Nm ( Nm (..)
          , TyName
          , eqName
          ) where

import qualified Data.Text     as T
import           Prettyprinter (Pretty (pretty))
import           U

data Nm a = Nm { name   :: T.Text
               , unique :: !U
               , loc    :: a
               } deriving (Functor)

-- for testing
eqName :: Nm a -> Nm a -> Bool
eqName (Nm n _ _) (Nm n' _ _) = n == n'

instance Eq (Nm a) where
    (==) (Nm _ u _) (Nm _ u' _) = u == u'

instance Pretty (Nm a) where
    pretty (Nm t _ _) = pretty t
    -- pretty (Nm t (U u) _) = pretty t <> pretty u

instance Show (Nm a) where show=show.pretty

instance Ord (Nm a) where
    compare (Nm _ u _) (Nm _ u' _) = compare u u'

type TyName = Nm
