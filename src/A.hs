{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module A ( E (..)
         , T (..)
         , TB (..)
         , BBin (..)
         , BTer (..)
         , BUn (..)
         , DfnVar (..)
         , D (..)
         , Program (..)
         , C (..)
         , N (..)
         , mapExpr
         , getFS, flushD
         -- * Base functors
         , EF (..)
         ) where

import           Control.Recursion  (Base, Corecursive, Recursive)
import qualified Data.ByteString    as BS
import qualified Data.IntMap        as IM
import           Data.Maybe         (listToMaybe)
import           Data.Semigroup     ((<>))
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector        as V
import           GHC.Generics       (Generic)
import           Nm
import           Prettyprinter      (Doc, Pretty (..), braces, brackets, concatWith, encloseSep, flatAlt, group, hardline, indent, parens, pipe, punctuate, tupled, (<+>))
import           Regex.Rure         (RurePtr)

infixr 6 <#>
infixr 6 <##>

(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

(<##>) :: Doc a -> Doc a -> Doc a
(<##>) x y = x <> hardline <> hardline <> y

data TB = TyInteger
        | TyFloat
        | TyStr | TyR
        | TyStream
        | TyVec
        | TyBool
        | TyOption
        | TyUnit
        deriving (Eq, Ord)

tupledByFunky :: Doc ann -> [Doc ann] -> Doc ann
tupledByFunky sep = group . encloseSep (flatAlt "⟨ " "⟨") (flatAlt " ⟩" "⟩") sep

tupledBy :: Doc ann -> [Doc ann] -> Doc ann
tupledBy sep = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") sep

jacTup :: Pretty a => [a] -> Doc ann
jacTup = tupledBy " . " . fmap pretty

data T = TyB { tyBuiltin :: TB }
       | TyApp { tyApp0 :: T, tyApp1 :: T }
       | TyArr { tyArr0 :: T, tyArr1 :: T }
       | TyVar { tyVar :: Nm () }
       | TyTup { tyTups :: [T] }
       | Rho { tyRho :: Nm (), tyArms :: IM.IntMap T }
       deriving (Eq, Ord)

instance Pretty TB where
    pretty TyInteger = "Integer"
    pretty TyStream  = "Stream"
    pretty TyBool    = "Bool"
    pretty TyStr     = "Str"
    pretty TyFloat   = "Float"
    pretty TyVec     = "List"
    pretty TyOption  = "Optional"
    pretty TyUnit    = "𝟙"
    pretty TyR       = "Regex"

instance Show TB where show=show.pretty

instance Pretty T where
    pretty (TyB b)        = pretty b
    pretty (TyApp ty ty') = pretty ty <+> pretty ty'
    pretty (TyVar n)      = pretty n
    pretty (TyArr ty ty') = pretty ty <+> "⟶" <+> pretty ty'
    pretty (TyTup tys)    = jacTup tys
    pretty (Rho n fs)     = braces (pretty n <+> pipe <+> prettyFields (IM.toList fs))

prettyFields :: [(Int, T)] -> Doc ann
prettyFields = mconcat . punctuate "," . fmap g where g (i, t) = pretty i <> ":" <+> pretty t

instance Show T where show=show.pretty

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
         | TallyList -- length of vector
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
    pretty TallyList  = "#*"

data BTer = ZipW
          | Fold | Scan
          | Substr
          | Option
          | Captures | AllCaptures
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
data BBin = Plus | Times | Div
          | Minus | Exp
          | Eq | Neq | Geq | Gt | Lt | Leq
          | Map
          | Matches -- ^ @'string' ~ /pat/@
          | NotMatches
          | And | Or
          | Min | Max
          | Split | Splitc
          | Prior
          | Filter
          | Sprintf
          | Match
          | MapMaybe
          | Fold1
          | DedupOn
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
    pretty Map        = "¨"
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
    pretty Exp        = "**"
    pretty DedupOn    = "~.*"

data DfnVar = X | Y deriving (Eq)

instance Pretty DfnVar where pretty X = "x"; pretty Y = "y"

-- 0-ary
data N = Ix | Nf | None | Fp deriving (Eq)

-- expression
data E a = Column { eLoc :: a, col :: Int }
         | IParseCol { eLoc :: a, col :: Int } -- always a column
         | FParseCol { eLoc :: a, col :: Int }
         | ParseCol { eLoc :: a, col :: Int }
         | Field { eLoc :: a, eField :: Int }
         | LastField { eLoc :: a }
         | AllField { eLoc :: a } -- ^ Think @$0@ in awk.
         | AllColumn { eLoc :: a } -- ^ Think @$0@ in awk.
         | EApp { eLoc :: a, eApp0 :: E a, eApp1 :: E a }
         | Guarded { eLoc :: a, eP :: E a, eGuarded :: E a }
         | Implicit { eLoc :: a, eImplicit :: E a }
         | Let { eLoc :: a, eBind :: (Nm a, E a), eE :: E a }
         -- TODO: literals type (make pattern matching easier down the road)
         | Var { eLoc :: a, eVar :: !(Nm a) }
         | ILit { eLoc :: a, eInt :: !Integer }
         | BLit { eLoc :: a, eBool :: !Bool }
         | StrLit { eLoc :: a, eStr :: BS.ByteString }
         | RegexLit { eLoc :: a, eRr :: BS.ByteString }
         | FLit { eLoc :: a, eFloat :: !Double }
         | Lam { eLoc :: a, eBound :: Nm a, lamE :: E a }
         | Dfn { eLoc :: a, eDfn :: E a }
         | BB { eLoc :: a, eBin :: BBin }
         | TB { eLoc :: a, eTer :: BTer }
         | UB { eLoc :: a, eUn :: BUn }
         | NB { eLoc :: a, eNil :: N }
         | Tup { eLoc :: a, esTup :: [E a] }
         | ResVar { eLoc :: a, dfnVar :: DfnVar }
         | RC RurePtr -- compiled regex after normalization
         | Arr { eLoc :: a, elems :: V.Vector (E a) }
         | Anchor { eLoc :: a, eAnchored :: [E a] }
         | Paren { eLoc :: a, eExpr :: E a }
         | OptionVal { eLoc :: a, eMaybe :: Maybe (E a) }
         | Cond { eLoc :: a, eIf :: E a, eThen :: E a, eElse :: E a }
         | In { oop :: E a, ip :: Maybe (E a), mm :: Maybe (E a), istream :: E a }
         deriving (Functor, Generic)

instance Recursive (E a) where

instance Corecursive (E a) where

data EF a x = ColumnF a Int
            | IParseColF a Int
            | FParseColF a Int
            | ParseColF a Int
            | FieldF a Int
            | LastFieldF a
            | AllFieldF a
            | AllColumnF a
            | EAppF a x x
            | GuardedF a x x
            | ImplicitF a x
            | LetF a (Nm a, x) x
            | VarF a (Nm a)
            | ILitF a Integer
            | BLitF a Bool
            | StrLitF a BS.ByteString
            | RegexLitF a BS.ByteString
            | FLitF a Double
            | LamF a (Nm a) x
            | DfnF a x
            | BBF a BBin
            | TBF a BTer
            | UBF a BUn
            | NBF a N
            | TupF a [x]
            | ResVarF a DfnVar
            | RCF RurePtr
            | ArrF a (V.Vector x)
            | AnchorF a [x]
            | ParenF a x
            | OptionValF a (Maybe x)
            | CondF a x x x
            | InF x (Maybe x) (Maybe x) x
            deriving (Generic, Functor, Foldable, Traversable)

type instance Base (E a) = (EF a)

instance Pretty N where
    pretty Ix = "⍳"; pretty Nf = "nf"; pretty None = "None"; pretty Fp = "fp"

instance Pretty (E a) where
    pretty (Column _ i)                                           = "$" <> pretty i
    pretty AllColumn{}                                            = "$0"
    pretty (IParseCol _ i)                                        = "$" <> pretty i <> ":i"
    pretty (FParseCol _ i)                                        = "$" <> pretty i <> ":f"
    pretty (ParseCol _ i)                                         = "$" <> pretty i <> ":"
    pretty AllField{}                                             = "`0"
    pretty (Field _ i)                                            = "`" <> pretty i
    pretty LastField{}                                            = "`*"
    pretty (EApp _ (EApp _ (BB _ Prior) e) e')                    = pretty e <> "\\." <+> pretty e'
    pretty (EApp _ (EApp _ (BB _ Max) e) e')                      = "max" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BB _ Min) e) e')                      = "min" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BB _ Split) e) e')                    = "split" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BB _ Splitc) e) e')                   = "splitc" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BB _ Match) e) e')                    = "match" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BB _ Sprintf) e) e')                  = "sprintf" <+> pretty e <+> pretty e'
    pretty (EApp _ (EApp _ (BB _ Map) e) e')                      = pretty e <> "¨" <> pretty e'
    pretty (EApp _ (EApp _ (BB _ b) e) e')                        = pretty e <+> pretty b <+> pretty e'
    pretty (EApp _ (BB _ b) e)                                    = parens (pretty e <> pretty b)
    pretty (EApp _ (EApp _ (EApp _ (TB _ Fold) e) e') e'')        = pretty e <> "|" <> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TB _ Scan) e) e') e'')        = pretty e <> "^" <> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TB _ ZipW) op) e') e'')       = "," <> pretty op <+> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TB _ Substr) e) e') e'')      = "substr" <+> pretty e <+> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TB _ Option) e) e') e'')      = "option" <+> pretty e <+> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TB _ AllCaptures) e) e') e'') = "captures" <+> pretty e <+> pretty e' <+> pretty e''
    pretty (EApp _ (EApp _ (EApp _ (TB _ Captures) e) e') e'')    = pretty e <+> "~*" <+> pretty e' <+> pretty e''
    pretty (EApp _ (UB _ (At i)) e)                               = pretty e <> "." <> pretty i
    pretty (EApp _ (UB _ (Select i)) e)                           = pretty e <> "->" <> pretty i
    pretty (EApp _ (UB _ IParse) e')                              = pretty e' <> ":i"
    pretty (EApp _ (UB _ FParse) e')                              = pretty e' <> ":f"
    pretty (EApp _ (UB _ Parse) e')                               = pretty e' <> ":"
    pretty (EApp _ e@UB{} e')                                     = pretty e <> pretty e'
    pretty (EApp _ e e')                                          = pretty e <+> pretty e'
    pretty (Var _ n)                                              = pretty n
    pretty (ILit _ i)                                             = pretty i
    pretty (RegexLit _ rr)                                        = "/" <> pretty (decodeUtf8 rr) <> "/"
    pretty (FLit _ f)                                             = pretty f
    pretty (BLit _ True)                                          = "#t"
    pretty (BLit _ False)                                         = "#f"
    pretty (BB _ b)                                               = parens (pretty b)
    pretty (UB _ u)                                               = pretty u
    pretty (StrLit _ str)                                         = pretty (decodeUtf8 str)
    pretty (ResVar _ x)                                           = pretty x
    pretty (Tup _ es)                                             = jacTup es
    pretty (Lam _ n e)                                            = parens ("λ" <> pretty n <> "." <+> pretty e)
    pretty (Dfn _ e)                                              = brackets (pretty e)
    pretty (Guarded _ p e)                                        = braces (pretty p) <> braces (pretty e)
    pretty (Implicit _ e)                                         = braces ("|" <+> pretty e)
    pretty (NB _ n)                                               = pretty n
    pretty RC{}                                                   = "(compiled regex)"
    pretty (Let _ (n, b) e)                                       = "let" <+> "val" <+> pretty n <+> ":=" <+> pretty b <+> "in" <+> pretty e <+> "end"
    pretty (Paren _ e)                                            = parens (pretty e)
    pretty (Arr _ es)                                             = tupledByFunky "," (V.toList $ pretty <$> es)
    pretty (Anchor _ es)                                          = "&" <> tupledBy "." (pretty <$> es)
    pretty (OptionVal _ (Just e))                                 = "Some" <+> pretty e
    pretty (OptionVal _ Nothing)                                  = "None"
    pretty (Cond _ e0 e1 e2)                                      = "if" <+> pretty e0 <+> "then" <+> pretty e1 <+> "else" <+> pretty e2

instance Show (E a) where show=show.pretty

-- for tests
instance Eq (E a) where
    (==) (Column _ i) (Column _ j)              = i == j
    (==) (IParseCol _ i) (IParseCol _ j)        = i == j
    (==) (FParseCol _ i) (FParseCol _ j)        = i == j
    (==) (Field _ i) (Field _ j)                = i == j
    (==) LastField{} LastField{}                = True
    (==) AllColumn{} AllColumn{}                = True
    (==) AllField{} AllField{}                  = True
    (==) (EApp _ e0 e1) (EApp _ e0' e1')        = e0 == e0' && e1 == e1'
    (==) (Guarded _ p e) (Guarded _ p' e')      = p == p' && e == e'
    (==) (Implicit _ e) (Implicit _ e')         = e == e'
    (==) (Let _ (n, eϵ) e) (Let _ (n', eϵ') e') = eqName n n' && e == e' && eϵ == eϵ'
    (==) (Var _ n) (Var _ n')                   = eqName n n'
    (==) (Lam _ n e) (Lam _ n' e')              = eqName n n' && e == e'
    (==) (ILit _ i) (ILit _ j)                  = i == j
    (==) (FLit _ u) (FLit _ v)                  = u == v
    (==) (StrLit _ str) (StrLit _ str')         = str == str'
    (==) (RegexLit _ rr) (RegexLit _ rr')       = rr == rr'
    (==) (BLit _ b) (BLit _ b')                 = b == b'
    (==) (BB _ b) (BB _ b')                     = b == b'
    (==) (TB _ b) (TB _ b')                     = b == b'
    (==) (UB _ unOp) (UB _ unOp')               = unOp == unOp'
    (==) (NB _ x) (NB _ y)                      = x == y
    (==) (Tup _ es) (Tup _ es')                 = es == es'
    (==) (ResVar _ x) (ResVar _ y)              = x == y
    (==) (Dfn _ f) (Dfn _ g)                    = f == g -- we're testing for lexical equivalence
    (==) RC{} _                                 = error "Cannot compare compiled regex!"
    (==) _ RC{}                                 = error "Cannot compare compiled regex!"
    (==) (Paren _ e) e'                         = e == e'
    (==) e (Paren _ e')                         = e == e'
    (==) _ _                                    = False

data C = IsNum | IsEq | IsOrd
       | IsParse | IsPrintf
       | IsSemigroup
       | Functor -- ^ For map (@"@)
       | Foldable | Witherable
       deriving (Eq, Ord)

instance Pretty C where
    pretty IsNum       = "Num"
    pretty IsEq        = "Eq"
    pretty IsOrd       = "Ord"
    pretty IsParse     = "Parseable"
    pretty IsSemigroup = "Semigroup"
    pretty Functor     = "Functor"
    pretty Foldable    = "Foldable"
    pretty IsPrintf    = "Printf"
    pretty Witherable  = "Witherable"

instance Show C where show=show.pretty

-- decl
data D a = SetFS T.Text
         | FunDecl (Nm a) [Nm a] (E a)
         | FlushDecl
         deriving (Functor)

instance Pretty (D a) where
    pretty (SetFS bs)       = ":set" <+> "/" <> pretty bs <> "/;"
    pretty (FunDecl n ns e) = "fn" <+> pretty n <> tupled (pretty <$> ns) <+> ":=" <#> indent 2 (pretty e <> ";")
    pretty FlushDecl        = ":flush;"

data Program a = Program { decls :: [D a], expr :: E a } deriving (Functor)

instance Pretty (Program a) where
    pretty (Program ds e) = concatWith (<##>) (pretty <$> ds) <##> pretty e

instance Show (Program a) where show=show.pretty

flushD :: Program a -> Bool
flushD (Program ds _) = any p ds where p FlushDecl = True; p _ = False

getFS :: Program a -> Maybe T.Text
getFS (Program ds _) = listToMaybe (concatMap go ds) where go (SetFS bs) = [bs]; go _ = []

mapExpr :: (E a -> E a) -> Program a -> Program a
mapExpr f (Program ds e) = Program ds (f e)
