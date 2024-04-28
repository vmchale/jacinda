{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Backend.T ( run ) where

import           A
import           Control.Exception                 (Exception, throw)
import           Control.Monad.State.Strict        (State, evalState, state)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Builder           (hPutBuilder)
import           Data.ByteString.Builder.RealFloat (doubleDec)
import           Data.Function                     ((&))
import qualified Data.IntMap.Strict                as IM
import           Data.List                         (foldl')
import           Data.Maybe                        (fromMaybe)
import qualified Data.Vector                       as V
import           Jacinda.Backend.Const
import           Jacinda.Regex
import           Nm
import           Prettyprinter                     (hardline, pretty)
import           Prettyprinter.Render.Text         (putDoc)
import           Regex.Rure                        (RurePtr)
import           System.IO                         (stdout)
import           U

data EvalErr = EmptyFold
             | IndexOutOfBounds Int
             | NoSuchField Int BS.ByteString
             | InternalCoercionError (E T) TB
             | ExpectedTup (E T)
             | InternalReg Tmp
             | InternalNm (Nm T)
             | InternalArityOrEta Int (E T)
             deriving (Show)

instance Exception EvalErr where

data StreamError = NakedField deriving (Show)

instance Exception StreamError where

type Env = IM.IntMap (Maybe (E T))
-- data Tmp = Main | IO | Tmp !Int
type Tmp = Int -- -1 = Main
type Β = IM.IntMap (E T)

(!) :: Env -> Tmp -> Maybe (E T)
(!) m r = IM.findWithDefault (throw$InternalReg r) r m

type MM = State Int

nI :: MM Int
nI = state (\i -> (i, i+1))

-- Β evaluate in little local context?
data IR = Wr Tmp (Maybe (E T)) | IO (Maybe (E T))
-- substitute line context after? hmm... e.g. columnize $0 as `0...

run :: RurePtr -> Bool -> Int -> E T -> [BS.ByteString] -> IO ()
run _ _ _ e _ | TyB TyStream:$_ <- eLoc e = undefined
run r _ _ e bs = pDocLn $ evalState (summar r e (-1) bs) 0

pDocLn :: E T -> IO ()
pDocLn (Lit _ (FLit f)) = hPutBuilder stdout (doubleDec f <> "\n")
pDocLn e                = putDoc (pretty e <> hardline)

summar :: RurePtr -> E T -> Tmp -> [BS.ByteString] -> MM (E T)
summar r e@(EApp _ (EApp _ (EApp _ (TB _ Fold) _) _) _) main bs = do
    -- TODO: Β environment? put in then evaluate them at the end idk.
    -- (still gotta take from the last but basically replace with hole-name,
    -- which is looked up to a temp, then... we go)
    (iEnv, g) <- φ e main
    let ctxs=zipWith (\ ~(x,y) z -> (x,y,z)) [(b, splitBy r b) | b <- bs] [1..]
        updates=g<$>ctxs
        finEnv=foldl' (&) iEnv updates
    pure $ fromMaybe (error "internal error??") $ IM.findWithDefault (throw$InternalReg main) main finEnv

(+@) :: (Env, LineCtx -> Env -> Env) -> (Env, LineCtx -> Env -> Env) -> (Env, LineCtx -> Env -> Env)
(s0, f) +@ (s1, g) = (s0<>s1, \l -> g l.f l)

φ :: E T -> Tmp -> MM (Env, LineCtx -> Env -> Env)
φ (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) tgt = do
    let iEnv=IM.singleton tgt (Just seed)
    t <- nI
    f <- ctx xs t
    let g=wF op t tgt
    pure (iEnv, (g.).f)

ctx :: E T -> Tmp -> MM (LineCtx -> Env -> Env)
ctx AllColumn{} res                        = pure $ \ ~(b, _, _) -> IM.insert res (Just$mkStr b)
ctx (EApp _ (EApp _ (BB _ Map) f) xs) o    = do {t <- nI; sb <- ctx xs t; pure (\l -> wM f t o.sb l)}
ctx (EApp _ (EApp _ (BB _ Filter) p) xs) o = do {t <- nI; sb <- ctx xs t; pure (\l -> wP p t o.sb l)}
ctx e _                                    = error (show e)

type LineCtx = (BS.ByteString, V.Vector BS.ByteString, Integer) -- line number

asR :: E T -> RurePtr
asR (RC r) = r; asR e = throw (InternalCoercionError e TyR)

asS :: E T -> BS.ByteString
asS (Lit _ (StrLit s)) = s; asS e = throw (InternalCoercionError e TyStr)

asI :: E T -> Integer
asI (Lit _ (ILit i)) = i; asI e = throw (InternalCoercionError e TyInteger)

asB :: E T -> Bool
asB (Lit _ (BLit b)) = b; asB e = throw (InternalCoercionError e TyBool)

(!>) :: Β -> Nm T -> E T
(!>) m n = IM.findWithDefault (throw $ InternalNm n) (unU$unique n) m

(@!) :: E T -> Β -> E T
e@Lit{} @! _   = e
e@RC{} @! _    = e
(Var _ n) @! b = b !> n
(EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Plus) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkI (asI x0e+asI x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Minus) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkI (asI x0e-asI x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Times) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkI (asI x0e*asI x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Eq) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkB (asS x0e==asS x1e)
(EApp _ (EApp _ (UB _ Const) x) _) @! b = x@!b
(EApp _ (EApp _ (BB _ Matches) s) r) @! b =
    let se=s@!b; re=r@!b
    in mkB (isMatch' (asR re) (asS se))
(EApp _ (EApp _ (BB _ NotMatches) s) r) @! b =
    let se=s@!b; re=r@!b
    in mkB (not$isMatch' (asR re) (asS se))

me :: [(Nm T, E T)] -> Β
me xs = IM.fromList [(unU$unique nm, e) | (nm, e) <- xs]

ms :: Nm T -> E T -> Β
ms (Nm _ (U i) _) = IM.singleton i

wM :: E T -> Tmp -> Tmp -> Env -> Env
wM (Lam _ n e) src tgt env =
    let xO=env!src
    in case xO of
        Just x ->
            let be=ms n x; y=e@!be
            in IM.insert tgt (Just y) env
        Nothing -> IM.insert tgt Nothing env
wM e _ _ _ = throw$InternalArityOrEta 1 e

wP :: E T -> Tmp -> Tmp -> Env -> Env
wP (Lam _ n e) src tgt env =
    let xO=env!src
    in case xO of
        Just x ->
            let be=ms n x; p=e@!be
            in IM.insert tgt (if asB p then Just x else Nothing) env
        Nothing -> IM.insert tgt Nothing env
wP e _ _ _ = throw $ InternalArityOrEta 1 e

wF :: E T -> Tmp -> Tmp -> Env -> Env
wF (Lam _ nacc (Lam _ nn e)) src tgt env =
    let accO = env ! tgt; xO = env ! src
    in case (accO, xO) of
        (Just acc, Just x) ->
            let be=me [(nacc, acc), (nn, x)]
                res=e@!be
            in IM.insert tgt (Just res) env
        (Just acc, Nothing) -> IM.insert tgt (Just acc) env
wF e _ _ _ = throw $ InternalArityOrEta 2 e
