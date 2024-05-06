{-# LANGUAGE OverloadedStrings #-}

module Jacinda.Backend.T ( run ) where

import           A
import           A.I
import           Control.Exception                 (Exception, throw)
import           Control.Monad.State.Strict        (State, evalState, state)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Builder           (hPutBuilder)
import           Data.ByteString.Builder.RealFloat (doubleDec)
import           Data.Foldable                     (fold, traverse_)
import           Data.Function                     ((&))
import qualified Data.IntMap.Strict                as IM
import           Data.List                         (foldl', scanl')
import           Data.Maybe                        (fromMaybe)
import qualified Data.Vector                       as V
import           Data.Word                         (Word8)
import           Jacinda.Backend.Const
import           Jacinda.Backend.Parse
import           Jacinda.Backend.Printf
import           Jacinda.Regex
import           Nm
import           Prettyprinter                     (hardline, pretty)
import           Prettyprinter.Render.Text         (putDoc)
import           Regex.Rure                        (RureMatch (RureMatch), RurePtr)
import           System.IO                         (hFlush, stdout)
import           Ty.Const
import           U

data EvalErr = EmptyFold
             | IndexOutOfBounds Int
             | NoSuchField Int BS.ByteString
             | InternalCoercionError (E T) TB
             | ExpectedTup (E T)
             | InternalTmp Tmp
             | InternalNm (Nm T)
             | InternalArityOrEta Int (E T)
             deriving (Show)

instance Exception EvalErr where

data StreamError = NakedField deriving (Show)

instance Exception StreamError where

-- TODO: dedup... tracking env!
type Env = IM.IntMap (Maybe (E T))
type Tmp = Int
type Β = IM.IntMap (E T)

at :: V.Vector a -> Int -> a
v `at` ix = case v V.!? (ix-1) of {Just x -> x; Nothing -> throw $ IndexOutOfBounds ix}

fieldOf :: V.Vector BS.ByteString -> BS.ByteString -> Int -> BS.ByteString
fieldOf fs b n = case fs V.!? (n-1) of {Just x -> x; Nothing -> throw $ NoSuchField n b}

parseAsEInt :: BS.ByteString -> E T
parseAsEInt = mkI.readDigits

parseAsF :: BS.ByteString -> E T
parseAsF = mkF.readFloat

(!) :: Env -> Tmp -> Maybe (E T)
(!) m r = IM.findWithDefault (throw$InternalTmp r) r m

type MM = State Int

nI :: MM Int
nI = state (\i -> (i, i+1))

nN :: T -> MM (Nm T)
nN t = do {u <- nI; pure (Nm "fold_hole" (U u) t)}

run :: RurePtr -> Bool -> Int -> E T -> [BS.ByteString] -> IO ()
run _ _ _ e _ | TyB TyUnit <- eLoc e = undefined
run r flush j e bs | TyB TyStream:$_ <- eLoc e = traverse_ (traverse_ (pS flush)).flip evalState j $ do
    t <- nI
    (iEnv, μ) <- ctx e t
    let ctxs=zipWith (\ ~(x,y) z -> (x,y,z)) [(b, splitBy r b) | b <- bs] [1..]
        outs=μ<$>ctxs; es=scanl' (&) iEnv outs
    pure ((! t)<$>es)
run r _ j e bs = pDocLn $ evalState (summar r e bs) j

pS p = if p then (*>fflush).pDocLn else pDocLn where fflush = hFlush stdout

pDocLn :: E T -> IO ()
pDocLn (Lit _ (FLit f)) = hPutBuilder stdout (doubleDec f <> "\n")
pDocLn e                = putDoc (pretty e <> hardline)

summar :: RurePtr -> E T -> [BS.ByteString] -> MM (E T)
summar r e bs = do
    (iEnv, g, e0) <- collect e
    let ctxs=zipWith (\ ~(x,y) z -> (x,y,z)) [(b, splitBy r b) | b <- bs] [1..]
        updates=g<$>ctxs
        finEnv=foldl' (&) iEnv updates
    u <- nI
    pure (e0@!(u,fromMaybe (throw EmptyFold)<$>finEnv))

collect :: E T -> MM (Env, LineCtx -> Env -> Env, E T)
collect e@(EApp ty (EApp _ (EApp _ (TB _ Fold) _) _) _) = do
    v <- nN ty
    (iEnv, g) <- φ e (unU$unique v)
    pure (iEnv, g, Var ty v)
collect e@(EApp ty (EApp _ (BB _ Fold1) _) _) = do
    v <- nN ty
    (iEnv, g) <- φ e (unU$unique v)
    pure (iEnv, g, Var ty v)
collect (Tup ty es) = do
    (seedEnvs, updates, es') <- unzip3 <$> traverse collect es
    pure (fold seedEnvs, ts updates, Tup ty es')
collect (EApp ty0 (EApp ty1 op@BB{} e0) e1) = do
    (env0, f0, e0') <- collect e0
    (env1, f1, e1') <- collect e1
    pure (env0<>env1, \l -> f1 l.f0 l, EApp ty0 (EApp ty1 op e0') e1')
collect (EApp ty0 f@UB{} e) = do
    (env, fϵ, eϵ) <- collect e
    pure (env, fϵ, EApp ty0 f eϵ)

ts :: [LineCtx -> Env -> Env] -> LineCtx -> Env -> Env
ts = foldl' (\f g l -> f l.g l) (const id)

φ :: E T -> Tmp -> MM (Env, LineCtx -> Env -> Env)
φ (EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) tgt = do
    let iEnv=IM.singleton tgt (Just$!seed)
    t <- nI
    (env, f) <- ctx xs t
    u <- nI
    let g=wF op u t tgt
    pure (env<>iEnv, (g.).f)
φ (EApp _ (EApp _ (EApp _ (TB _ Scan) op) seed) xs) tgt = do
    let iEnv=IM.singleton tgt (Just$!seed)
    t <- nI
    (env, f) <- ctx xs t
    u <- nI
    let g=wF op u t tgt
    pure (env<>iEnv, (g.).f)
φ (EApp _ (EApp _ (BB _ Fold1) op) xs) tgt = do
    let iEnv=IM.singleton tgt Nothing
    t <- nI
    (env, f) <- ctx xs t
    u <- nI
    let g=wF op u t tgt
    pure (env<>iEnv, (g.).f)

κ :: E T -> LineCtx -> E T
κ AllField{} ~(b, _, _)   = mkStr b
κ (Field _ i) ~(_, bs, _) = mkStr $ bs `at` i
κ LastField{} ~(_, bs, _) = mkStr $ V.last bs
κ FieldList{} ~(_, bs, _) = vS bs
κ (EApp ty e0 e1) line    = EApp ty (e0 `κ` line) (e1 `κ` line)
κ (NB _ Ix) ~(_, _, fp)   = mkI fp
κ e@BB{} _                = e
κ e@UB{} _                = e
κ e@TB{} _                = e
κ e@Lit{} _               = e
κ e@RC{} _                = e

ni t=IM.singleton t Nothing
na=IM.alter (\k -> case k of {Nothing -> Just Nothing; Just x -> Just x})

ctx :: E T -> Tmp -> MM (Env, LineCtx -> Env -> Env)
ctx AllColumn{} res                                     = pure (ni res, \ ~(b, _, _) -> IM.insert res (Just$!mkStr b))
ctx FParseAllCol{} res                                  = pure (ni res, \ ~(b, _, _) -> IM.insert res (Just$!parseAsF b))
ctx IParseAllCol{} res                                  = pure (ni res, \ ~(b, _, _) -> IM.insert res (Just$!parseAsEInt b))
ctx (FParseCol _ i) res                                 = pure (ni res, \ ~(b, bs, _) -> IM.insert res (Just$!parseAsF (fieldOf bs b i)))
ctx (IParseCol _ i) res                                 = pure (ni res, \ ~(b, bs, _) -> IM.insert res (Just$!parseAsEInt (fieldOf bs b i)))
ctx (ParseCol (_:$TyB TyFloat) i) res                   = pure (ni res, \ ~(b, bs, _) -> IM.insert res (Just$!parseAsF (fieldOf bs b i)))
ctx (ParseCol (_:$TyB TyInteger) i) res                 = pure (ni res, \ ~(b, bs, _) -> IM.insert res (Just$!parseAsEInt (fieldOf bs b i)))
ctx (EApp _ (EApp _ (BB _ Map) f) xs) o                 = do {t <- nI; (env, sb) <- ctx xs t; u <- nI; pure (na o env, \l->wM f u t o.sb l)}
ctx (EApp _ (EApp _ (BB _ MapMaybe) f) xs) o            = do {t <- nI; (env, sb) <- ctx xs t; u <- nI; pure (na o env, \l->wMM f u t o.sb l)}
ctx (EApp _ (UB _ CatMaybes) xs) o                      = do {t <- nI; (env, sb) <- ctx xs t; pure (na o env, \l->wCM t o.sb l)}
ctx (EApp _ (EApp _ (BB _ Filter) p) xs) o              = do {t <- nI; (env, sb) <- ctx xs t; u <- nI; pure (na o env, \l->wP p u t o.sb l)}
ctx (Guarded _ p e) o                                   = do {u <- nI; pure (ni o, wG (p, e) u o)}
ctx (Implicit _ e) o                                    = do {u <- nI; pure (ni o, wI e u o)}
ctx (EApp _ (EApp _ (EApp _ (TB _ Scan) op) seed) xs) o = do {t <- nI; (env, sb) <- ctx xs t; u <- nI; pure (IM.insert o (Just$!seed) env, \l->wF op u t o.sb l)}

type LineCtx = (BS.ByteString, V.Vector BS.ByteString, Integer) -- line number

asS :: E T -> BS.ByteString
asS (Lit _ (StrLit s)) = s; asS e = throw (InternalCoercionError e TyStr)

asI :: E T -> Integer
asI (Lit _ (ILit i)) = i; asI e = throw (InternalCoercionError e TyInteger)

asF :: E T -> Double
asF (Lit _ (FLit x)) = x; asF e = throw (InternalCoercionError e TyFloat)

asR :: E T -> RurePtr
asR (RC r) = r; asR e = throw (InternalCoercionError e TyR)

asM :: E T -> Maybe (E T)
asM (OptionVal _ e) = e; asM e = throw (InternalCoercionError e TyOption)

asB :: E T -> Bool
asB (Lit _ (BLit b)) = b; asB e = throw (InternalCoercionError e TyBool)

asV :: E T -> V.Vector (E T)
asV (Arr _ v) = v; asV e = throw (InternalCoercionError e TyVec)

asT :: E T -> [E T]
asT (Tup _ es) = es; asT e = throw (ExpectedTup e)

vS :: V.Vector BS.ByteString -> E T
vS = Arr (tyV tyStr).fmap mkStr

the :: BS.ByteString -> Word8
the bs = case BS.uncons bs of
    Nothing                -> error "Empty splitc char!"
    Just (c,b) | BS.null b -> c
    Just _                 -> error "Splitc takes only one char!"

asTup :: Maybe RureMatch -> E T
asTup Nothing                = OptionVal undefined Nothing
asTup (Just (RureMatch s e)) = OptionVal undefined (Just (Tup undefined (mkI . fromIntegral <$> [s, e])))

(!>) :: Β -> Nm T -> E T
(!>) m n = IM.findWithDefault (throw $ InternalNm n) (unU$unique n) m

a2e :: (Int, Β) -> E T -> E T -> E T -> RM T (E T)
a2e b op e0 e1 = (@!b)<$>a2 op e0 e1

a1e :: (Int, Β) -> E T -> E T -> RM T (E T)
a1e b f x = (@!b)<$>a1 f x

a1 :: E T -> E T -> RM T (E T)
a1 f x | TyArr _ cod <- eLoc f = bM (EApp cod f x)

a2 :: E T -> E T -> E T -> RM T (E T)
a2 op x0 x1 | TyArr _ t@(TyArr _ t') <- eLoc op = bM (EApp t' (EApp t op x0) x1)

num :: Num a => BBin -> Maybe (a -> a -> a)
num Plus = Just (+); num Minus = Just (-); num Times = Just (*); num _ = Nothing

binRel :: Ord a => BBin -> Maybe (a -> a -> Bool)
binRel Lt = Just (<); binRel Gt = Just (>); binRel Eq = Just (==)
binRel Neq = Just (/=); binRel Geq = Just (>=); binRel Leq = Just (<=)
binRel _   = Nothing

(@!) :: E T -> (Int, Β) -> E T
e@Lit{} @! _   = e
e@RC{} @! _    = e
(Var _ n) @! (_, b) = b!>n
(EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Max) x0) x1) @! b =
    let x0'=asI (x0@!b); x1'=asI (x1@!b)
    in mkI (max x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) Min) x0) x1) @! b =
    let x0'=asI (x0@!b); x1'=asI (x1@!b)
    in mkI (min x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Max) x0) x1) @! b =
    let x0'=asF (x0@!b); x1'=asF (x1@!b)
    in mkF (max x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) Min) x0) x1) @! b =
    let x0'=asF (x0@!b); x1'=asF (x1@!b)
    in mkF (min x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Max) x0) x1) @! b =
    let x0'=asS (x0@!b); x1'=asS (x1@!b)
    in mkStr (max x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Min) x0) x1) @! b =
    let x0'=asS (x0@!b); x1'=asS (x1@!b)
    in mkStr (min x0' x1')
(EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) op) x0) x1) @! b | Just op' <- num op =
    let x0e=asI (x0@!b); x1e=asI (x1@!b)
    in mkI (op' x0e x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) op) x0) x1) @! b | Just op' <- num op =
    let x0e=asF (x0@!b); x1e=asF (x1@!b)
    in mkF (op' x0e x1e)
(EApp _ (EApp _ (BB _ Div) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkF (asF x0e/asF x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyInteger) _) op) x0) x1) @! b | Just rel <- binRel op =
    let x0e=asI (x0@!b); x1e=asI (x1@!b)
    in mkB (rel x0e x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyFloat) _) op) x0) x1) @! b | Just rel <- binRel op =
    let x0e=asF (x0@!b); x1e=asF (x1@!b)
    in mkB (rel x0e x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) op) x0) x1) @! b | Just rel <- binRel op =
    let x0e=asS (x0@!b); x1e=asS (x1@!b)
    in mkB (rel x0e x1e)
(EApp _ (EApp _ (BB (TyArr (TyB TyStr) _) Plus) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkStr (asS x0e<>asS x1e)
(EApp _ (EApp _ (BB _ And) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkB (asB x0e&&asB x1e)
(EApp _ (EApp _ (BB _ Or) x0) x1) @! b =
    let x0e=x0@!b; x1e=x1@!b
    in mkB (asB x0e||asB x1e)
(EApp _ (EApp _ (UB _ Const) x) _) @! b = x@!b
(EApp _ (EApp _ (BB _ Match) s) r) @! b =
    let s'=s@!b; r'=r@!b
    in asTup (find' (asR r') (asS s'))
(EApp _ (EApp _ (BB _ Matches) s) r) @! b =
    let se=s@!b; re=r@!b
    in mkB (isMatch' (asR re) (asS se))
(EApp _ (EApp _ (BB _ NotMatches) s) r) @! b =
    let se=s@!b; re=r@!b
    in mkB (not$isMatch' (asR re) (asS se))
(Tup ty es) @! b = Tup ty ((@!b)<$>es)
(EApp _ (UB _ Tally) e) @! b =
    let e'=e@!b
        r=fromIntegral (BS.length$asS e')
    in mkI r
(EApp _ (UB _ TallyList) e) @! b =
    let e'=e@!b
        r=fromIntegral (V.length$asV e')
    in mkI r
(EApp _ (EApp _ (BB _ Sprintf) fs) s) @! b =
    let fs'=fs@!b; s'=s@!b
    in mkStr (sprintf (asS fs') s')
(Cond _ p e e') @! b = let p'=p@!b in if asB p' then e@!b else e'@!b
(EApp ty (EApp _ (EApp _ (TB _ Captures) s) i) r) @! b =
    let s'=s@!b; i'=i@!b; r'=r@!b
    in OptionVal ty (mkStr <$> findCapture (asR r') (asS s') (fromIntegral$asI i'))
(EApp ty (EApp _ (EApp _ (TB _ AllCaptures) s) i) r) @! b =
    let s'=s@!b; i'=i@!b; r'=r@!b
    in Arr ty (V.fromList (mkStr <$> captures' (asR r') (asS s') (fromIntegral$asI i')))
(NB (TyB TyStr) MZ) @! _ = mkStr BS.empty
(NB ty@(TyB TyVec:$_) MZ) @! _ = Arr ty V.empty
(EApp _ (UB _ Not) e) @! b = let e'=asB (e@!b) in mkB (not e')
(EApp _ (EApp _ (BB _ Split) s) r) @! b =
    let s'=asS (s@!b); r'=asR (r@!b)
    in vS (splitBy r' s')
(EApp _ (EApp _ (BB _ Splitc) s) c) @! b =
    let s'=asS (s@!b); c'=the$asS (c@!b)
    in vS (V.fromList (BS.split c' s'))
(EApp _ (UB _ FParse) x) @! b = let x'=x@!b in parseAsF (asS (x'@!b))
(EApp _ (UB _ IParse) x) @! b = let x'=x@!b in parseAsEInt (asS (x'@!b))
(EApp (TyB TyInteger) (UB _ Parse) x) @! b = let x'=x@!b in parseAsEInt (asS (x'@!b))
(EApp (TyB TyFloat) (UB _ Parse) x) @! b = let x'=x@!b in parseAsF (asS (x'@!b))
(EApp _ (UB _ (At i)) v) @! b = let v'=v@!b in asV v' `at` (i-1)
(EApp _ (UB _ (Select i)) x) @! b = let x'=x@!b in asT x' !! (i-1)
(EApp _ (UB _ Floor) x) @! b = mkI (floor (asF (x@!b)))
(EApp _ (UB _ Ceiling) x) @! b = mkI (ceiling (asF (x@!b)))
(EApp (TyB TyInteger) (UB _ Negate) i) @! b = mkI (negate (asI (i@!b)))
(EApp (TyB TyFloat) (UB _ Negate) x) @! b = mkF (negate (asF (x@!b)))
(EApp ty (UB _ Some) e) @! b = OptionVal ty (Just (e@!b))
(NB ty None) @! _ = OptionVal ty Nothing
(EApp _ (EApp _ (EApp _ (TB _ Substr) s) i0) i1) @! b =
    let i0'=i0@!b; i1'=i1@!b; s'=s@!b
    in mkStr (substr (asS s') (fromIntegral$asI i0') (fromIntegral$asI i1'))
(EApp _ (EApp _ (EApp _ (TB _ Sub1) r) s0) s1) @! b =
    let r'=r@!b; s0'=s0@!b; s1'=s1@!b
    in mkStr (sub1 (asR r') (asS s1') (asS s0'))
(EApp _ (EApp _ (EApp _ (TB _ Subs) r) s0) s1) @! b =
    let r'=r@!b; s0'=s0@!b; s1'=s1@!b
    in mkStr (subs (asR r') (asS s1') (asS s0'))
(EApp _ (EApp _ (EApp _ (TB _ Fold) op) seed) xs) @! b@(k,_) | TyB TyVec:$_ <- eLoc xs =
    let seed'=seed@!b; xs'=xs@!b
    in βa k $ V.foldM (a2e b op) seed' (asV xs')
(EApp _ (EApp _ (BB _ Fold1) op) xs) @! b@(k,_) | TyB TyVec:$_ <- eLoc xs =
    let xs'=xs@!b; xsV=asV xs'
        (seed, xs'') = case V.uncons xsV of
            Just v  -> v
            Nothing -> throw EmptyFold
    in βa k $ V.foldM (a2e b op) seed xs''
(EApp yT@(TyB TyVec:$_) (EApp _ (BB _ Filter) p) xs) @! b@(k,_) =
    let xs'=xs@!b
    in Arr yT (βa k $ V.filterM (fmap asB.a1e b p) (asV xs'))
(EApp yT@(TyB TyVec:$_) (EApp _ (BB _ Map) f) xs) @! b@(k,_) =
    let xs'=xs@!b
    in Arr yT (βa k $ traverse (a1e b f) (asV xs'))
(EApp yT@(TyB TyOption:$_) (EApp _ (BB _ Map) f) x) @! b@(k,_) =
    let x'=x@!b
    in OptionVal yT (βa k $ traverse (a1e b f) (asM x'))
(EApp yT@(TyB TyVec:$_) (EApp _ (BB _ MapMaybe) g) x) @! b@(k,_) =
    let x'=x@!b
    in Arr yT (βa k $ V.mapMaybeM (fmap asM.a1e b g) (asV x'))
(EApp yT@(TyB TyVec:$_) (UB _ CatMaybes) x) @! b =
    let x'=x@!b
    in Arr yT (V.catMaybes (asM<$>asV x'))
(EApp t (EApp _ (EApp _ (TB _ Option) x) g) y) @! b@(k,_) =
    let x'=x@!b; y'=y@!b
    in case asM y' of
        Nothing -> x'
        Just yϵ -> (@!b) $ βa k (bM (EApp t g yϵ))

me :: [(Nm T, E T)] -> Β
me xs = IM.fromList [(unU$unique nm, e) | (nm, e) <- xs]

ms :: Nm T -> E T -> Β
ms (Nm _ (U i) _) = IM.singleton i

wCM :: Tmp -> Tmp -> Env -> Env
wCM src tgt env =
    let xϵ=env!src
    in case xϵ of
        Just y  -> case asM y of {Nothing -> IM.insert tgt Nothing env; Just yϵ -> IM.insert tgt (Just$!yϵ) env}
        Nothing -> IM.insert tgt Nothing env

wMM :: E T -> Int -> Tmp -> Tmp -> Env -> Env
wMM (Lam _ n e) j src tgt env =
    let xϵ=env!src
    in case xϵ of
        Just x ->
            let be=ms n x; y=e@!(j,be)
            in case asM y of
                Just yϵ -> IM.insert tgt (Just$!yϵ) env
                Nothing -> IM.insert tgt Nothing env
        Nothing -> IM.insert tgt Nothing env
wMM e _ _ _ _ = throw$InternalArityOrEta 1 e

wM :: E T -> Int -> Tmp -> Tmp -> Env -> Env
wM (Lam _ n e) j src tgt env =
    let xϵ=env!src
    in case xϵ of
        Just x ->
            let be=ms n x; y=e@!(j,be)
            in IM.insert tgt (Just$!y) env
        Nothing -> IM.insert tgt Nothing env
wM e _ _ _ _ = throw$InternalArityOrEta 1 e

wI :: E T -> Int -> Tmp -> LineCtx -> Env -> Env
wI e j tgt line env =
    let e'=e `κ` line in IM.insert tgt (Just$!(e'@!(j,mempty))) env

wG :: (E T, E T) -> Int -> Tmp -> LineCtx -> Env -> Env
wG (p, e) j tgt line env =
    let p'=p `κ` line; p''=p'@!(j,mempty)
    in if asB p''
        then let e'=e `κ` line; e''=e'@!(j,mempty) in IM.insert tgt (Just$!e'') env
        else IM.insert tgt Nothing env

wP :: E T -> Int -> Tmp -> Tmp -> Env -> Env
wP (Lam _ n e) j src tgt env =
    let xϵ=env!src
    in case xϵ of
        Just x ->
            let be=ms n x; p=e@!(j,be)
            in IM.insert tgt (if asB p then Just$!x else Nothing) env
        Nothing -> IM.insert tgt Nothing env
wP e _ _ _ _ = throw $ InternalArityOrEta 1 e

wF :: E T -> Int -> Tmp -> Tmp -> Env -> Env
wF (Lam _ nacc (Lam _ nn e)) j src tgt env =
    let accϵ = env ! tgt; xϵ = env ! src
    in case (accϵ, xϵ) of
        (Just acc, Just x) ->
            let be=me [(nacc, acc), (nn, x)]
                res=e@!(j,be)
            in IM.insert tgt (Just$!res) env
        (Just acc, Nothing) -> IM.insert tgt (Just$!acc) env
        (Nothing, Nothing) -> IM.insert tgt Nothing env
        (Nothing, Just x) -> IM.insert tgt (Just$!x) env
wF e _ _ _ _ = throw $ InternalArityOrEta 2 e
