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
                   , D (..)
                   , Program (..)
                   , C (..)
                   , N (..)
                   , mapExpr
                   , getFS
                   -- * Base functors
                   , EF (..)
                   ) where

import           Control.Recursion  (Base, Corecursive, Recursive)
import qualified Data.ByteString    as BS
import           Data.Maybe         (listToMaybe)
import           Data.Semigroup     ((<>))
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector        as V
import           GHC.Generics       (Generic)
import           Intern.Name
import           Prettyprinter      (Doc, Pretty (..), braces, brackets, concatWith, encloseSep, flatAlt, group, hardline, indent, parens, tupled, (<+>))
import           Regex.Rure         (RurePtr)

infixr 6 <#>
infixr 6 <##>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

(<##>) :: Doc a -> Doc a -> Doc a
(<##>) x y = x <> hardline <> hardline <> y

-- kind
data K = Star
       | KArr K K
       deriving (Eq, Ord)

instance Pretty K where
    pretty Star         = "★"
    pretty (KArr k0 k1) = parens (pretty k0 <+> "⟶" <+> pretty k1)

data TB = TyInteger
        | TyFloat
        | TyDate
        | TyStr
        | TyStream
        | TyVec
        | TyBool
        | TyOption
        | TyUnit
        -- TODO: tyRegex
        -- TODO: convert float to int
        deriving (Eq, Ord)

-- unicode mathematical angle bracket
tupledByFunky :: Doc ann -> [Doc ann] -> Doc ann
tupledByFunky sep = group . encloseSep (flatAlt "⟨ " "⟨") (flatAlt " ⟩" "⟩") sep

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
    pretty TyVec     = "List"
    pretty TyOption  = "Optional"
    pretty TyUnit    = "𝟙"

instance Pretty (T a) where
    pretty (TyB _ b)        = pretty b
    pretty (TyApp _ ty ty') = pretty ty <+> pretty ty'
    pretty (TyVar _ n)      = pretty n
    pretty (TyArr _ ty ty') = pretty ty <+> "⟶" <+> pretty ty'
    pretty (TyTup _ tys)    = jacTup tys
    pretty (TyNamed _ tn)   = pretty tn

instance Show (T a) where
    show = show . pretty

-- unary
data BUn = Tally -- length of string field
         | Const
         | Not -- ^ Boolean
         | At Int
         | Select Int
         | IParse
         | FParse
         | Parse
         | Floor
         | Ceiling
         | Some
         | Dedup
         | CatMaybes
         | Negate
         deriving (Eq)

instance Pretty BUn where
    pretty Tally      = "#"
    pretty Const      = "[:"
    pretty Not        = "!"
    pretty (At i)     = "." <> pretty i
    pretty (Select i) = "->" <> pretty i
    pretty IParse     = ":i"
    pretty FParse     = ":f"
    pretty Floor      = "floor"
    pretty Ceiling    = "ceil"
    pretty Parse      = ":"
    pretty Some       = "Some"
    pretty Dedup      = "~."
    pretty CatMaybes  = ".?"
    pretty Negate     = "-."

-- ternary
data BTer = ZipW
          | Fold
          | Scan
          | Substr
          | Option
          | Captures
          | AllCaptures
          deriving (Eq)

instance Pretty BTer where
    pretty ZipW        = ","
    pretty Fold        = "|"
    pretty Scan        = "^"
    pretty Substr      = "substr"
    pretty Option      = "option"
    pretty Captures    = "~*"
    pretty AllCaptures = "captures"

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
          | Split
          | Splitc
          | Prior
          | Filter
          | Sprintf
          | Match
          | MapMaybe
          | Fold1
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
    pretty Prior      = "\\."
    pretty Filter     = "#."
    pretty Split      = "split"
    pretty Splitc     = "splitc"
    pretty Sprintf    = "sprintf"
    pretty Match      = "match"
    pretty MapMaybe   = ":?"
    pretty Fold1      = "|>"

data DfnVar = X | Y deriving (Eq)

instance Pretty DfnVar where
    pretty X = "x"
    pretty Y = "y"

-- 0-ary
data N = Ix
       | Nf
       | None
       | Fp
       deriving (Eq)

-- expression
data E a = Column { eLoc :: a, col :: Int }
         | IParseCol { eLoc :: a, col :: Int } -- always a column
         | FParseCol { eLoc :: a, col :: Int }
         | Field { eLoc :: a, eField :: Int }
         | LastField { eLoc :: a }
         | AllField { eLoc :: a } -- ^ Think @$0@ in awk.
         | AllColumn { eLoc :: a } -- ^ Think @$0@ in awk.
         | EApp { eLoc :: a, eApp0 :: E a, eApp1 :: E a }
         | Guarded { eLoc :: a, eP :: E a, eGuarded :: E a }
         | Implicit { eLoc :: a, eImplicit :: E a }
         | Let { eLoc :: a, eBind :: (Name a, E a), eE :: E a }
         -- TODO: literals type (make pattern matching easier down the road)
         | Var { eLoc :: a, eVar :: Name a }
         | IntLit { eLoc :: a, eInt :: !Integer }
         | BoolLit { eLoc :: a, eBool :: !Bool }
         | StrLit { eLoc :: a, eStr :: BS.ByteString }
         | RegexLit { eLoc :: a, eRr :: BS.ByteString }
         | FloatLit { eLoc :: a, eFloat :: !Double }
         | Lam { eLoc :: a, eBound :: Name a, lamE :: E a }
         | Dfn { eLoc :: a, eDfn :: E a } -- to be rewritten as a lambda...
         -- TODO: builtin sum type ? (makes pattern matching easier down the road)
         | BBuiltin { eLoc :: a, eBin :: BBin }
         | TBuiltin { eLoc :: a, eTer :: BTer }
         | UBuiltin { eLoc :: a, eUn :: BUn }
         | NBuiltin { eLoc :: a, eNil :: N }
         | Tup { eLoc :: a, esTup :: [E a] }
         | ResVar { eLoc :: a, dfnVar :: DfnVar }
         | RegexCompiled RurePtr -- holds compiled regex (after normalization)
         | Arr { eLoc :: a, elems :: V.Vector (E a) }
         | Anchor { eLoc :: a, eAnchored :: [E a] }
         | Paren { eLoc :: a, eExpr :: E a }
         | OptionVal { eLoc :: a, eMaybe :: Maybe (E a) }
         | Cond { eLoc :: a, eIf :: E a, eThen :: E a, eElse :: E a }
         -- TODO: regex literal
         deriving (Functor, Generic)
         -- TODO: side effects: allow since it's strict?

instance Recursive (E a) where

instance Corecursive (E a) where

data EF a x = ColumnF a Int
            | IParseColF a Int
            | FParseColF a Int
            | FieldF a Int
            | LastFieldF a
            | AllFieldF a
            | AllColumnF a
            | EAppF a x x
            | GuardedF a x x
            | ImplicitF a x
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
            | NBuiltinF a N
            | TupF a [x]
            | ResVarF a DfnVar
            | RegexCompiledF RurePtr
            | ArrF a (V.Vector x)
            | AnchorF a [x]
            | ParenF a x
            | OptionValF a (Maybe x)
            | CondF a x x x
            deriving (Generic, Functor, Foldable, Traversable)

type instance Base (E a) = (EF a)

instance Pretty N where
    pretty Ix   = "ix"
    pretty Nf   = "nf"
    pretty None = "None"
    pretty Fp   = "fp"

instance Pretty (E a) where
    pretty (Column _ i)                                                 = "$" <> pretty i
    pretty AllColumn{}                                                  = "$0"
    pretty (IParseCol _ i)                                              = "$" <> pretty i <> ":i"
    pretty (FParseCol _ i)                                              = "$" <> pretty i <> ":f"
    pretty AllField{}                                                   = "`0"
    pretty (Field _ i)                                                  = "`" <> pretty i
    pretty LastField{}                                                  = "`*"
    pretty (EApp _ (EApp _ (BBuiltin _ Prior) e) e')                    = pretty e <> "\\." <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Max) e) e')                      = "max" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Min) e) e')                      = "min" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Split) e) e')                    = "split" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Splitc) e) e')                   = "splitc" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Match) e) e')                    = "match" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Sprintf) e) e')                  = "sprintf" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ Map) e) e')                      = pretty e <> "\"" <> pretty e'
    pretty (EApp _ (EApp _ (BBuiltin _ b) e) e')                        = pretty e <+> pretty b <+> pretty e'
    pretty (EApp _ (BBuiltin _ b) e)                                    = parens (pretty e <> pretty b)
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ Fold) e) e') e'')        = pretty e <> "|" <> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ Scan) e) e') e'')        = pretty e <> "^" <> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ ZipW) op) e') e'')       = "," <> pretty op <+> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ Substr) e) e') e'')      = "substr" <+> pretty e <+> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ Option) e) e') e'')      = "option" <+> pretty e <+> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ AllCaptures) e) e') e'') = "captures" <+> pretty e <+> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TBuiltin _ Captures) e) e') e'')    = pretty e <+> "~*" <+> pretty e' <+> pretty e''
    pretty (EApp _ (UBuiltin _ (At i)) e)                               = pretty e <> "." <> pretty i
    pretty (EApp _ (UBuiltin _ (Select i)) e)                           = pretty e <> "->" <> pretty i
    pretty (EApp _ (UBuiltin _ IParse) e')                              = pretty e' <> ":i"
    pretty (EApp _ (UBuiltin _ FParse) e')                              = pretty e' <> ":f"
    pretty (EApp _ (UBuiltin _ Parse) e')                               = pretty e' <> ":"
    pretty (EApp _ e@UBuiltin{} e')                                     = pretty e <> pretty e'
    pretty (EApp _ e e')                                                = pretty e <+> pretty e'
    pretty (Var _ n)                                                    = pretty n
    pretty (IntLit _ i)                                                 = pretty i
    pretty (RegexLit _ rr)                                              = "/" <> pretty (decodeUtf8 rr) <> "/"
    pretty (FloatLit _ f)                                               = pretty f
    pretty (BoolLit _ True)                                             = "#t"
    pretty (BoolLit _ False)                                            = "#f"
    pretty (BBuiltin _ b)                                               = parens (pretty b)
    pretty (UBuiltin _ u)                                               = pretty u
    pretty (StrLit _ bstr)                                              = pretty (decodeUtf8 bstr)
    pretty (ResVar _ x)                                                 = pretty x
    pretty (Tup _ es)                                                   = jacTup es
    pretty (Lam _ n e)                                                  = parens ("λ" <> pretty n <> "." <+> pretty e)
    pretty (Dfn _ e)                                                    = brackets (pretty e)
    pretty (Guarded _ p e)                                              = braces (pretty p) <> braces (pretty e)
    pretty (Implicit _ e)                                               = braces ("|" <+> pretty e)
    pretty (NBuiltin _ n)                                               = pretty n
    pretty RegexCompiled{}                                              = "(compiled regex)"
    pretty (Let _ (n, b) e)                                             = "let" <+> "val" <+> pretty n <+> ":=" <+> pretty b <+> "in" <+> pretty e <+> "end"
    pretty (Paren _ e)                                                  = parens (pretty e)
    pretty (Arr _ es)                                                   = tupledByFunky "," (V.toList $ pretty <$> es)
    pretty (Anchor _ es)                                                = "&" <> tupledBy "." (pretty <$> es)
    pretty (OptionVal _ (Just e))                                       = "Some" <+> pretty e
    pretty (OptionVal _ Nothing)                                        = "None"
    pretty (Cond _ e0 e1 e2)                                            = "if" <+> pretty e0 <+> "then" <+> pretty e1 <+> "else" <+> pretty e2

instance Show (E a) where
    show = show . pretty

-- for tests
instance Eq (E a) where
    (==) (Column _ i) (Column _ j)              = i == j
    (==) (IParseCol _ i) (IParseCol _ j)        = i == j
    (==) (FParseCol _ i) (FParseCol _ j)        = i == j
    (==) (Field _ i) (Field _ j)                = i == j
    (==) LastField{} LastField{}               = True
    (==) AllColumn{} AllColumn{}                = True
    (==) AllField{} AllField{}                  = True
    (==) (EApp _ e0 e1) (EApp _ e0' e1')        = e0 == e0' && e1 == e1'
    (==) (Guarded _ p e) (Guarded _ p' e')      = p == p' && e == e'
    (==) (Implicit _ e) (Implicit _ e')         = e == e'
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
    (==) (NBuiltin _ x) (NBuiltin _ y)          = x == y
    (==) (Tup _ es) (Tup _ es')                 = es == es'
    (==) (ResVar _ x) (ResVar _ y)              = x == y
    (==) (Dfn _ f) (Dfn _ g)                    = f == g -- we're testing for lexical equivalence
    (==) RegexCompiled{} _                      = error "Cannot compare compiled regex!"
    (==) _ RegexCompiled{}                      = error "Cannot compare compiled regex!"
    (==) (Paren _ e) e'                         = e == e'
    (==) e (Paren _ e')                         = e == e'
    (==) _ _                                    = False

data C = IsNum
       | IsEq
       | IsOrd
       | IsParseable
       | IsSemigroup
       | Functor -- ^ For map (@"@)
       | Foldable
       | IsPrintf
       | HasField Int (T K)
       | Witherable
       -- TODO: witherable
       deriving (Eq, Ord)

instance Pretty C where
    pretty IsNum           = "Num"
    pretty IsEq            = "Eq"
    pretty IsOrd           = "Ord"
    pretty IsParseable     = "Parseable"
    pretty IsSemigroup     = "Semigroup"
    pretty Functor         = "Functor"
    pretty Foldable        = "Foldable"
    pretty IsPrintf        = "Printf"
    pretty (HasField i ty) = "HasField" <+> pretty i <+> "~" <+> pretty ty
    pretty Witherable      = "Witherable"

instance Show C where
    show = show . pretty

-- decl
data D a = SetFS BS.ByteString
         | FunDecl (Name a) [Name a] (E a)
         deriving (Functor)

instance Pretty (D a) where
    pretty (SetFS bs)       = ":set" <+> "/" <> pretty (decodeUtf8 bs) <> "/"
    pretty (FunDecl n ns e) = "fn" <+> pretty n <> tupled (pretty <$> ns) <+> ":=" <#> indent 2 (pretty e <> ";")

-- TODO: fun decls (type decls)
data Program a = Program { decls :: [D a], expr :: E a } deriving (Functor)

instance Pretty (Program a) where
    pretty (Program ds e) = concatWith (<##>) (pretty <$> ds) <##> pretty e

instance Show (Program a) where
    show = show . pretty

getFS :: Program a -> Maybe BS.ByteString
getFS (Program ds _) = listToMaybe (concatMap go ds) where
    go (SetFS bs) = [bs]
    go _          = []

mapExpr :: (E a -> E a) -> Program a -> Program a
mapExpr f (Program ds e) = Program ds (f e)
