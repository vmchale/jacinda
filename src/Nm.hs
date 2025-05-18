module Nm ( Nm (..), TyName ) where

import qualified Data.Text     as T
import           Prettyprinter (Pretty (pretty))
import           U

data Nm a = Nm { name   :: T.Text
               , unique :: !U
               , loc    :: a
               } deriving (Functor)

instance Eq (Nm a) where
    (==) (Nm _ u _) (Nm _ u' _) = u == u'

instance Ord (Nm a) where
    compare (Nm _ u _) (Nm _ u' _) = compare u u'

instance Pretty (Nm a) where
    pretty (Nm t _ _) = pretty t

instance Show (Nm a) where show=show.pretty

type TyName = Nm
