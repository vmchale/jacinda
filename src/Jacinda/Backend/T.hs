module Jacinda.Backend.T ( wF ) where

import           A
import           Control.Exception          (Exception, throw)
import           Control.Monad.State.Strict (State, state)
import qualified Data.ByteString            as BS
import qualified Data.IntMap.Strict         as IM
import           Data.List                  (foldl')
import qualified Data.Vector                as V
import           Jacinda.Backend.Const
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
-- data Tmp = Main | IO | Tmp !Int
type Tmp = Int
type Β = IM.IntMap (E T)

(!) :: Env -> Tmp -> Maybe (E T)
(!) m r = IM.findWithDefault (throw $ InternalReg r) r m

type MM = State Int

nI :: MM Int
nI = state (\i -> (i, i+1))

data IR = Wr Tmp (Maybe (E T)) | IO (Maybe (E T))

col :: E T -> Tmp -> LineCtx -> Env -> Env
col AllColumn{} res ~(b, _, _) = IM.insert res (Just$mkStr b)

type LineCtx = (BS.ByteString, V.Vector BS.ByteString, Integer) -- line number

asI :: E T -> Integer
asI (Lit _ (ILit i)) = i; asI e = throw (InternalCoercionError e TyInteger)

asB :: E T -> Bool
asB (Lit _ (BLit b)) = b; asB e = throw (InternalCoercionError e TyBool)

(!>) :: Β -> Nm T -> E T
(!>) m n = IM.findWithDefault (throw $ InternalNm n) (unU$unique n) m

(@!) :: E T -> Β -> E T
e@Lit{} @! _   = e
(Var _ n) @! b = b !> n
(EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Plus) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkI (asI x0e+asI x1e)

me :: [(Nm T, E T)] -> Β
me xs = IM.fromList [(unU$unique nm, e) | (nm, e) <- xs]

ms :: Nm T -> E T -> Β
ms (Nm _ (U i) _) e = IM.singleton i e

wM :: E T -> Tmp -> Tmp -> Env -> Env
wM (Lam _ n e) src tgt env =
    let xO=env!src
    in case xO of
        Just x ->
            let be=ms n x; y=e@!be
            in IM.insert tgt (Just y) env
        Nothing -> IM.insert tgt Nothing env

wP :: E T -> Tmp -> Tmp -> Env -> Env
wP (Lam _ n e) src tgt env =
    let xO=env!src
    in case xO of
        Just x ->
            let be=ms n x; p=e@!be
            in IM.insert tgt (if asB p then Just x else Nothing) env
        Nothing -> IM.insert tgt Nothing env

wF :: E T -> Tmp -> Tmp -> Env -> Env
wF (Lam _ nacc (Lam _ nn e)) src tgt env =
    let accO = env ! tgt; xO = env ! src
    in case (accO, xO) of
        (Just acc, Just x) ->
            let be=me [(nacc, acc), (nn, x)]
                res=e@!be
            in IM.insert tgt (Just res) env
        (Just acc, Nothing) -> IM.insert tgt (Just acc) env
