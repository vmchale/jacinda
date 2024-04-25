module Jacinda.Backend.T ( wF ) where

import           A
import           Control.Exception  (Exception, throw)
import qualified Data.ByteString    as BS
import qualified Data.IntMap.Strict as IM
import qualified Data.Vector        as V
import           Data.Word          (Word64)
import           Nm
import           U

data EvalErr = EmptyFold
             | IndexOutOfBounds Int
             | NoSuchField Int BS.ByteString
             | InternalCoercionError (E T) TB
             | ExpectedTup (E T)
             | InternalReg Tmp
             | InternalNm (Nm T)
             deriving (Show)

instance Exception EvalErr where

type Env = IM.IntMap (Maybe (E T))
type Tmp = Int
type Β = IM.IntMap (E T)

(!) :: Env -> Tmp -> Maybe (E T)
(!) m r = IM.findWithDefault (throw $ InternalReg r) r m

type LineCtx = (BS.ByteString, V.Vector BS.ByteString, Integer) -- line number

asI :: E T -> Integer
asI (Lit _ (ILit i)) = i; asI e = throw (InternalCoercionError e TyInteger)

(!>) :: Β -> Nm T -> E T
(!>) m n = IM.findWithDefault (throw $ InternalNm n) (unU$unique n) m

(@!) :: E T -> Β -> E T
e@Lit{} @! _ = e

bind :: Nm T -> E T -> Β -> Β
bind (Nm _ (U u) _) e = IM.insert u e

wP :: E T -> Tmp -> Tmp -> Env -> Env
wP = undefined

wF :: E T -> Tmp -> Tmp -> Env -> Env
wF (Lam _ nacc (Lam _ nn e)) src tgt env =
    let accO = env ! tgt; xO = env ! src
    in case (accO, xO) of
        (Just acc, Just x) ->
            let be=bind nacc acc (bind nn x mempty)
                res=e@!be
            in IM.insert tgt (Just res) env
        (Just acc, Nothing) -> env
