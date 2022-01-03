{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Jacinda.AST ( E (..)
                   , T (..)
                   , TB (..)
                   , BBin (..)
                   , BTer (..)
                   , BUn (..)
                   , K (..)
                   , DfnVar (..)
                   -- * Base functors
                   , EF (..)
                   ) where

import           Control.Recursion  (Base, Corecursive, Recursive)
import qualified Data.ByteString    as BS
import           Data.Semigroup     ((<>))
import           Data.Text.Encoding (decodeUtf8)
import           GHC.Generics       (Generic)
import           Intern.Name
import           Prettyprinter      (Doc, Pretty (..), braces, brackets, encloseSep, flatAlt, group, parens, (<+>))
import           Regex.Rure         (RurePtr)

-- kind
data K = Star
       | KArr K K
       deriving (Eq, Ord)

data TB = TyInteger
        | TyFloat
        | TyDate
        | TyStr
        | TyStream
        | TyBool
        -- TODO: convert float to int
        deriving (Eq, Ord)

tupledBy :: Doc ann -> [Doc ann] -> Doc ann
tupledBy sep = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") sep

jacTup :: Pretty a => [a] -> Doc ann
jacTup = tupledBy " . " . fmap pretty

-- type
data T a = TyNamed { tLoc :: a, tyName :: TyName a }
         | TyB { tLoc :: a, tyBuiltin :: TB }
         | TyApp { tLoc :: a, tyApp0 :: T a, tyApp1 :: T a }
         | TyArr { tLoc :: a, tyArr0 :: T a, tyArr1 :: T a }
         | TyVar { tLoc :: a, tyVar :: Name a }
         | TyTup { tLoc :: a, tyTups :: [T a] } -- in practice, parse only >1
         deriving (Eq, Ord, Functor) -- this is so we can store consntraints in a set, not alpha-equiv. or anything
         -- TODO: type vars, products...

instance Pretty TB where
    pretty TyInteger = "Integer"
    pretty TyStream  = "Stream"
    pretty TyBool    = "Bool"
    pretty TyStr     = "Str"
    pretty TyFloat   = "Float"
    pretty TyDate    = "Date"

instance Pretty (T a) where
    pretty (TyB _ b)        = pretty b
    pretty (TyApp _ ty ty') = pretty ty <+> pretty ty'
    pretty (TyVar _ n)      = "'" <> pretty n -- type variables are ticked
    pretty (TyArr _ ty ty') = pretty ty <+> "->" <+> pretty ty' -- tODO: unicode arrows
    pretty (TyTup _ tys)    = jacTup tys

instance Show (T a) where
    show = show . pretty

-- unary
data BUn = Tally -- length of string field
         | Const
         | Not -- ^ Boolean
         deriving (Eq)

instance Pretty BUn where
    pretty Tally = "#"
    pretty Const = "[:"
    pretty Not   = "!"

-- ternary
data BTer = ZipW
          | Fold
          | Scan
          deriving (Eq)

instance Pretty BTer where
    pretty ZipW = ","
    pretty Fold = "|"
    pretty Scan = "^"

-- builtin
data BBin = Plus
          | Times
          | Div
          | Minus
          | Eq
          | Neq
          | Geq
          | Gt
          | Lt
          | Leq
          | Map
          | Matches -- ^ @/pat/ ~ 'string'@
          | NotMatches
          | And
          | Or
          | Min
          | Max
          | Prior
          -- TODO: floor functions, sqrt, sin, cos, exp. (power)
          deriving (Eq)

instance Pretty BBin where
    pretty Plus       = "+"
    pretty Times      = "*"
    pretty Div        = "%"
    pretty Minus      = "-"
    pretty Eq         = "="
    pretty Gt         = ">"
    pretty Lt         = "<"
    pretty Geq        = ">="
    pretty Leq        = "<="
    pretty Neq        = "!="
    pretty Map        = "\""
    pretty Matches    = "~"
    pretty NotMatches = "!~"
    pretty And        = "&"
    pretty Or         = "||"
    pretty Max        = "max"
    pretty Min        = "min"
    pretty Prior      = "\\"

data DfnVar = X | Y deriving (Eq)

instance Pretty DfnVar where
    pretty X = "x"
    pretty Y = "y"

-- expression
data E a = Column { eLoc :: a, col :: Int }
         | IParseCol { eLoc :: a, col :: Int } -- always a column
         | FParseCol { eLoc :: a, col :: Int }
         | Field { eLoc :: a, field :: Int }
         | IParseField { eLoc :: a, field :: Int }
         | FParseField { eLoc :: a, field :: Int }
         | AllField { eLoc :: a } -- ^ Think @$0@ in awk.
         | AllColumn { eLoc :: a } -- ^ Think @$0@ in awk.
         | EApp { eLoc :: a, eApp0 :: E a, eApp1 :: E a }
         | Guarded { eLoc :: a, eP :: E a, eGuarded :: E a }
         | Let { eLoc :: a, eBind :: (Name a, E a), eE :: E a }
         -- TODO: literals type (make pattern matching easier down the road)
         | Var { eLoc :: a, eVar :: Name a }
         | IntLit { eLoc :: a, eInt :: Integer }
         | BoolLit { eLoc :: a, eBool :: Bool }
         | StrLit { eLoc :: a, eStr :: BS.ByteString }
         | RegexLit { eLoc :: a, eRr :: BS.ByteString }
         | FloatLit { eLoc :: a, eFloat :: Double }
         | Lam { eLoc :: a, eBound :: Name a, lamE :: E a }
         | Dfn { eLoc :: a, eDfn :: E a } -- to be rewritten as a lambda...
         -- TODO: builtin sum type ? (makes pattern matching easier down the road)
         | BBuiltin { eLoc :: a, eBin :: BBin }
         | TBuiltin { eLoc :: a, eTer :: BTer }
         | UBuiltin { eLoc :: a, eUn :: BUn }
         | Ix { eLoc :: a } -- only 0-ary builtin atm
         | Tup { eLoc :: a, esTup :: [E a] }
         | ResVar { eLoc :: a, dfnVar :: DfnVar }
         | RegexCompiled RurePtr -- holds compiled regex (after normalization)
         -- TODO: regex literal
         deriving (Functor, Generic)
         -- TODO: side effects: allow since it's strict?

instance Recursive (E a) where

instance Corecursive (E a) where

data EF a x = ColumnF a Int
            | IParseColF a Int
            | FParseColF a Int
            | FieldF a Int
            | IParseFieldF a Int
            | FParseFieldF a Int
            | AllFieldF a
            | AllColumnF a
            | EAppF a x x
            | GuardedF a x x
            | LetF a (Name a, x) x
            | VarF a (Name a)
            | IntLitF a Integer
            | BoolLitF a Bool
            | StrLitF a BS.ByteString
            | RegexLitF a BS.ByteString
            | FloatLitF a Double
            | LamF a (Name a) x
            | DfnF a x
            | BBuiltinF a BBin
            | TBuiltinF a BTer
            | UBuiltinF a BUn
            | IxF a
            | TupF a [x]
            | ResVarF a DfnVar
            | RegexCompiledF RurePtr
            deriving (Generic, Functor, Foldable, Traversable)

type instance Base (E a) = (EF a)

instance Pretty (E a) where
    pretty (Column _ i)                                           = "$" <> pretty i
    pretty AllColumn{}                                            = "$0"
    pretty (IParseCol _ i)                                        = "$" <> pretty i <> ":i"
    pretty (FParseCol _ i)                                        = "$" <> pretty i <> ":f"
    pretty AllField{}                                             = "`0"
    pretty (Field _ i)                                            = "`" <> pretty i
    pretty (IParseField _ i)                                      = "`" <> pretty i <> ":i"
    pretty (FParseField _ i)                                      = "`" <> pretty i <> ":f"
    pretty (EApp _ (EApp _ (BBuiltin _ Prior) e) e')              = pretty e <> "\\" <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Max) e) e')                = "max" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Min) e) e')                = "min" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ b) e) e')                  = pretty e <> pretty b <> pretty e'
    pretty (EApp _ (BBuiltin _ b) e)                              = parens (pretty b <> pretty e)
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ Fold) e) e') e'')  = pretty e <> "|" <> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) e) e') e'')  = pretty e <> "^" <> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) op) e') e'') = "," <> pretty op <+> pretty e' <+> pretty e''
    pretty (EApp _ e@UBuiltin{} e')                               = pretty e <> pretty e'
    pretty (EApp _ e e')                                          = pretty e <+> pretty e'
    pretty (Var _ n)                                              = pretty n
    pretty (IntLit _ i)                                           = pretty i
    pretty (RegexLit _ rr)                                        = "/" <> pretty (decodeUtf8 rr) <> "/"
    pretty (FloatLit _ f)                                         = pretty f
    pretty (BoolLit _ True)                                       = "#t"
    pretty (BoolLit _ False)                                      = "#f"
    pretty (BBuiltin _ b)                                         = parens (pretty b)
    pretty (UBuiltin _ u)                                         = pretty u
    pretty (StrLit _ bstr)                                        = pretty (decodeUtf8 bstr)
    pretty (ResVar _ x)                                           = pretty x
    pretty (Tup _ es)                                             = jacTup es
    pretty (Lam _ n e)                                            = "\\" <> pretty n <> "." <+> pretty e
    pretty (Dfn _ e)                                              = brackets (pretty e)
    pretty (Guarded _ p e)                                        = braces (pretty p) <> braces (pretty e)
    pretty Ix{}                                                   = "ix"
    pretty RegexCompiled{}                                        = error "Nonsense."

instance Show (E a) where
    show = show . pretty

-- for tests
instance Eq (E a) where
    (==) (Column _ i) (Column _ j)              = i == j
    (==) (IParseCol _ i) (IParseCol _ j)        = i == j
    (==) (FParseCol _ i) (FParseCol _ j)        = i == j
    (==) (Field _ i) (Field _ j)                = i == j
    (==) (IParseField _ i) (IParseField _ j)    = i == j
    (==) (FParseField _ i) (FParseField _ j)    = i == j
    (==) AllColumn{} AllColumn{}                = True
    (==) AllField{} AllField{}                  = True
    (==) (EApp _ e0 e1) (EApp _ e0' e1')        = e0 == e0' && e1 == e1'
    (==) (Guarded _ p e) (Guarded _ p' e')      = p == p' && e == e'
    (==) (Let _ (n, eϵ) e) (Let _ (n', eϵ') e') = eqName n n' && e == e' && eϵ == eϵ'
    (==) (Var _ n) (Var _ n')                   = eqName n n'
    (==) (Lam _ n e) (Lam _ n' e')              = eqName n n' && e == e'
    (==) (IntLit _ i) (IntLit _ j)              = i == j
    (==) (FloatLit _ u) (FloatLit _ v)          = u == v
    (==) (StrLit _ str) (StrLit _ str')         = str == str'
    (==) (RegexLit _ rr) (RegexLit _ rr')       = rr == rr'
    (==) (BoolLit _ b) (BoolLit _ b')           = b == b'
    (==) (BBuiltin _ b) (BBuiltin _ b')         = b == b'
    (==) (TBuiltin _ b) (TBuiltin _ b')         = b == b'
    (==) (UBuiltin _ unOp) (UBuiltin _ unOp')   = unOp == unOp'
    (==) (Tup _ es) (Tup _ es')                 = es == es'
    (==) (ResVar _ x) (ResVar _ y)              = x == y
    (==) (Dfn _ f) (Dfn _ g)                    = f == g -- we're testing for lexical equivalence
    (==) Ix{} Ix{}                              = True
    (==) RegexCompiled{} _                      = error "Cannot compare compiled regex!"
    (==) _ RegexCompiled{}                      = error "Cannot compare compiled regex!"
    (==) _ _                                    = False

-- TODO: decls/type decls
-- data Program a = Program [D a] (E a)
