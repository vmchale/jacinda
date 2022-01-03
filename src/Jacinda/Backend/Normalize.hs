-- TODO: test this module?
module Jacinda.Backend.Normalize ( eNorm
                                 , compileR
                                 ) where

import           Control.Recursion (cata, cataM, embed)
import qualified Data.ByteString   as BS
import           Jacinda.AST
import           Jacinda.Regex
import           Jacinda.Ty.Const

-- fill in regex with compiled.
compileR :: E (T K)
         -> E (T K)
compileR = cata a where
    a (RegexLitF _ rr) = RegexCompiled (compileDefault rr)
    a x                = embed x

-- will need a state/context at some point (let &c.)
eNorm :: E (T K)
      -> E (T K)
eNorm = cata a where
    a e@FieldF{}       = embed e
    a e@IParseFieldF{} = embed e
    a e@FParseFieldF{} = embed e
    a e@IntLitF{}      = embed e
    a e@FloatLitF{}    = embed e
    a e@BoolLitF{}     = embed e
    a e@StrLitF{}      = embed e
    a e@RegexLitF{}    = embed e
    a e@RegexCompiledF{} = embed e
    a e@UBuiltinF{}    = embed e
    a e@ColumnF{}      = embed e
    a e@AllColumnF{}   = embed e
    a e@IParseColF{}   = embed e
    a e@FParseColF{}   = embed e
    a e@AllFieldF{}    = embed e
    a e@GuardedF{}     = embed e
    a e@LamF{}         = embed e
    a e@BBuiltinF{}    = embed e
    a e@TBuiltinF{}    = embed e
    a e@TupF{}         = embed e
    a e@IxF{}          = embed e
    a e@(EAppF _ BBuiltin{} _) = embed e
    a e0@(EAppF _ (EApp _ (BBuiltin _ Matches) e) e') =
        case (e, e') of
            (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (isMatch' re str)
            (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (isMatch' re str)
            _                                -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin _ NotMatches) e) e') =
        case (e, e') of
            (RegexCompiled re, StrLit _ str) -> BoolLit tyBool (not $ isMatch' re str)
            (StrLit _ str, RegexCompiled re) -> BoolLit tyBool (not $ isMatch' re str)
            _                                -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Plus) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> IntLit tyI (i+j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Max) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> IntLit tyI (max i j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Min) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> IntLit tyI (min i j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Max) e) e') =
        case (e, e') of
            (FloatLit _ x, FloatLit _ y) -> FloatLit tyF (max x y)
            _                            -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Min) e) e') =
        case (e, e') of
            (FloatLit _ x, FloatLit _ y) -> FloatLit tyF (min x y)
            _                            -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Minus) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> IntLit tyI (i-j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Times) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> IntLit tyI (i*j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Plus) e) e') =
        case (e, e') of
            (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i+j)
            _                            -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Minus) e) e') =
        case (e, e') of
            (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i-j)
            _                            -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Times) e) e') =
        case (e, e') of
            (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i*j)
            _                            -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyFloat) _) Div) e) e') =
        case (e, e') of
            (FloatLit _ i, FloatLit _ j) -> FloatLit tyF (i/j)
            _                            -> embed e0
    a e0@(EAppF _ (UBuiltin _ Tally) e) =
        case e of
            StrLit _ str -> IntLit tyI (fromIntegral $ BS.length str)
            _            -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Lt) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i < j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Gt) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i > j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Eq) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i == j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Eq) e) e') =
        case (e, e') of
            (StrLit _ i, StrLit _ j) -> BoolLit tyBool (i == j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyInteger) _) Neq) e) e') =
        case (e, e') of
            (IntLit _ i, IntLit _ j) -> BoolLit tyBool (i /= j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin (TyArr _ (TyB _ TyStr) _) Neq) e) e') =
        case (e, e') of
            (StrLit _ i, StrLit _ j) -> BoolLit tyBool (i /= j)
            _                        -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin _ And) e) e') =
        case (e, e') of
            (BoolLit _ b, BoolLit _ b') -> BoolLit tyBool (b && b')
            _                           -> embed e0
    a e0@(EAppF _ (EApp _ (BBuiltin _ Or) e) e') =
        case (e, e') of
            (BoolLit _ b, BoolLit _ b') -> BoolLit tyBool (b || b')
            _                           -> embed e0
    a (EAppF _ (EApp _ (UBuiltin _ Const) e) _) = e
    a e@(EAppF _ (UBuiltin _ Const) _) = embed e
