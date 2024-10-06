{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module A ( E (..), T (..), (~>), TB (..), C (..)
         , L (..), N (..), BBin (..), BTer (..)
         , BUn (..), DfnVar (..)
         , D (..), Program (..)
         , Mode (..)
         , mapExpr
         , getS, flushD
         ) where

import           Control.DeepSeq    (NFData)
import qualified Data.ByteString    as BS
import qualified Data.IntMap        as IM
import           Data.List          (foldl')
import           Data.Maybe         (isJust)
import qualified Data.Text          as T
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Vector        as V
import           GHC.Generics       (Generic)
import           Nm
import           Nm.Map             (NmMap)
import qualified Nm.Map             as Nm
import           Prettyprinter      (Doc, Pretty (..), braces, brackets, concatWith, encloseSep, flatAlt, group, hardline, indent, parens, pipe, punctuate, tupled, (<+>))
import           Regex.Rure         (RurePtr)

infixr 6 <#>
infixr 6 <##>

(<#>), (<##>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y
(<##>) x y = x <> hardline <> hardline <> y

data TB = TyI | TyFloat | TyStr
        | TyStream | TyVec | TyOption
        | TyR | TyBool | TyUnit
        deriving (Eq, Ord)

tupledByFunky :: Doc ann -> [Doc ann] -> Doc ann
tupledByFunky sep = group . encloseSep (flatAlt "⟨ " "⟨") (flatAlt " ⟩" "⟩") sep

tupledBy :: Doc ann -> [Doc ann] -> Doc ann
tupledBy sep = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") sep

j'Tup :: Pretty a => [a] -> Doc ann
j'Tup = tupledBy " . " . fmap pretty

jrec = encloseSep (flatAlt "#{ " "#{") (flatAlt (hardline <+> "}") "}") (flatAlt " ; " "; ")

infixr 0 ~>

(~>) :: T -> T -> T
(~>) = TyArr

infixr 0 :$

data T = TyB { tyBuiltin :: TB }
       | (:$) { tyApp0, tyApp1 :: T }
       | TyArr { tyArr0, tyArr1 :: T }
       | TyVar { tyVar :: Nm () }
       | TyTup { tyTups :: [T] }
       | TyRec { tyres :: [(Nm (), T)] }
       | Rho { tyRho :: Nm (), tyArms :: IM.IntMap T }
       | Ρ { tyΡ :: Nm (), tyρs :: NmMap T }
       deriving Eq

instance Pretty TB where
    pretty TyI = "Integer"; pretty TyStr = "Str"; pretty TyFloat = "Float"
    pretty TyStream = "Stream"; pretty TyVec = "List"; pretty TyOption = "Optional"
    pretty TyBool = "Bool"; pretty TyUnit = "𝟙"; pretty TyR = "Regex"

instance Show TB where show=show.pretty

instance Pretty T where
    pretty (TyB b)        = pretty b
    pretty (ty:$ty')      = pretty ty <+> pretty ty'
    pretty (TyVar n)      = pretty n
    pretty (TyArr ty ty') = pretty ty <+> "⟶" <+> pretty ty'
    pretty (TyTup tys)    = j'Tup tys
    pretty (TyRec rs)     = jrec ((\(n,t) -> pretty n <> ":" <+> pretty t)<$>rs)
    pretty (Rho n fs)     = braces (pretty n <+> pipe <+> prettyFields (IM.toList fs))
    pretty (Ρ n fs)       = braces (pretty n <+> pipe <+> prettyFields (Nm.toList fs))

parensp True=parens; parensp False=id

prettyFields :: Pretty a => [(a, T)] -> Doc ann
prettyFields = mconcat . punctuate "," . fmap g where g (i, t) = pretty i <> ":" <+> pretty t

instance Show T where show=show.pretty

data BUn = Tally -- length of string field
         | Const
         | Not -- ^ Boolean
         | At !Int | Select !Int | SelR !(Nm ())
         | IParse | FParse | Parse
         | Floor | Ceiling
         | Some
         | Dedup | CatMaybes
         | Negate
         | TallyList -- length of vector
         | Head | Tail | Init | Last
         deriving (Eq)

instance Pretty BUn where
    pretty Tally      = "#"
    pretty Const      = "[:"
    pretty Not        = "!"
    pretty (At i)     = "." <> pretty i
    pretty (Select i) = "->" <> pretty i
    pretty (SelR r)   = "->" <> pretty r
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
    pretty Head       = "head#"
    pretty Tail       = "tail#"
    pretty Init       = "init#"
    pretty Last       = "last#"

data BTer = ZipW
          | Fold | Scan
          | Substr | Sub1 | Subs
          | Option
          | Captures | AllCaptures | Ixes
          | Bookend
          deriving (Eq)

instance Pretty BTer where
    pretty ZipW        = ","
    pretty Fold        = "|"
    pretty Scan        = "^"
    pretty Substr      = "substr"
    pretty Option      = "option"
    pretty Captures    = "~*"
    pretty AllCaptures = "captures"
    pretty Sub1        = "sub1"
    pretty Subs        = "subs"
    pretty Bookend     = ",,"
    pretty Ixes        = "ices"

-- builtin
data BBin = Plus | Times | Div
          | Minus | Exp
          | Eq | Neq | Geq | Gt | Lt | Leq
          | Map
          | Matches -- ^ @'string' ~ /pat/@
          | NotMatches | MMatch
          | And | Or
          | Min | Max
          | Split | Splitc
          | Prior | DedupOn | MapMaybe
          | Filter | Fold1
          | Match | Sprintf
          | Report
          | Take | Drop
          | Rein | Nier
          deriving (Eq)

instance Pretty BBin where
    pretty Plus = "+"; pretty Times = "*"; pretty Div = "%"; pretty Minus = "-"
    pretty Eq = "="; pretty Gt = ">"; pretty Lt = "<"; pretty Geq = ">="
    pretty Leq = "<="; pretty Neq = "!="; pretty Map = "¨"; pretty Matches = "~"
    pretty MMatch = "~?"; pretty NotMatches = "!~"; pretty And = "&"; pretty Or = "||"
    pretty Max = "max"; pretty Min = "min"; pretty Prior = "\\."; pretty Filter = "#."
    pretty Split = "split"; pretty Splitc = "splitc"; pretty Sprintf = "sprintf"
    pretty Match = "match"; pretty MapMaybe = ":?"; pretty Fold1 = "|>"
    pretty Exp = "**"; pretty DedupOn = "~.*"; pretty Report = "$>"
    pretty Take = "take#"; pretty Drop = "drop#"; pretty Rein = "reintercalate"
    pretty Nier = "@@"

data DfnVar = X | Y deriving (Eq)

instance Pretty DfnVar where pretty X="x"; pretty Y="y"

-- 0-ary
data N = Ix | Nf | None | Fp | MZ deriving (Eq)

data L = ILit !Integer | FLit !Double | BLit !Bool | StrLit BS.ByteString deriving (Generic, NFData, Eq)

class PS a where ps :: Int -> a -> Doc ann

-- expression
data E a = Column { eLoc :: a, col :: Int }
         | IParseCol { eLoc :: a, col :: Int } | FParseCol { eLoc :: a, col :: Int } | ParseCol { eLoc :: a, col :: Int }
         | Field { eLoc :: a, eField :: Int } | LastField { eLoc :: a } | FieldList { eLoc :: a }
         | AllField { eLoc :: a } -- ^ Think @$0@ in awk.
         | AllColumn { eLoc :: a } -- ^ Think @$0@ in awk.
         | IParseAllCol { eLoc :: a } -- ^ @$0@, parsed as an integer
         | FParseAllCol { eLoc :: a } -- ^ @$0@, parsed as a float
         | ParseAllCol { eLoc :: a }
         | EApp { eLoc :: a, eApp0, eApp1 :: E a }
         | Guarded { eLoc :: a, eP, eGuarded :: E a }
         | Implicit { eLoc :: a, eImplicit :: E a }
         | Let { eLoc :: a, eBind :: (Nm a, E a), eE :: E a }
         -- TODO: literals type (make pattern matching easier down the road)
         | Var { eLoc :: a, eVar :: !(Nm a) }
         | F { hole :: !(Nm a) }
         | Lit { eLoc :: a, lit :: !L }
         | RegexLit { eLoc :: a, eRr :: BS.ByteString }
         | Lam { eLoc :: a, eBound :: Nm a, lamE :: E a }
         | Dfn { eLoc :: a, eDfn :: E a }
         | BB { eLoc :: a, eBin :: BBin } | TB { eLoc :: a, eTer :: BTer } | UB { eLoc :: a, eUn :: BUn }
         | NB { eLoc :: a, eNil :: N }
         | Tup { eLoc :: a, esTup :: [E a] }
         | Rec { eLoc :: a, esR :: [(Nm a, E a)] }
         | ResVar { eLoc :: a, dfnVar :: DfnVar }
         | RC RurePtr -- compiled regex after normalization
         | Arr { eLoc :: a, elems :: !(V.Vector (E a)) }
         | Anchor { eLoc :: a, eAnchored :: [E a] }
         | Paren { eLoc :: a, eExpr :: E a }
         | OptionVal { eLoc :: a, eMaybe :: Maybe (E a) }
         | Cond { eLoc :: a, eIf, eThen, eElse :: E a }
         | RwB { eLoc :: a, eBin :: BBin } | RwT { eLoc :: a, eTer :: BTer }
         deriving (Functor)

instance Pretty N where
    pretty Ix="⍳"; pretty Nf="nf"; pretty None="None"; pretty Fp="fp"; pretty MZ="⍬"

instance Pretty L where
    pretty (ILit i)     = pretty i
    pretty (FLit d)     = pretty d
    pretty (BLit True)  = "#t"
    pretty (BLit False) = "#f"
    pretty (StrLit str) = pretty (decodeUtf8 str)

mPrec :: BBin -> Maybe Int
mPrec Prior      = Just 5
mPrec DedupOn    = Just 5
mPrec MapMaybe   = Just 5
mPrec Map        = Just 5
mPrec Filter     = Just 5
mPrec Fold1      = Just 5
mPrec Exp        = Just 8
mPrec Plus       = Just 6
mPrec Minus      = Just 6
mPrec Times      = Just 7
mPrec Div        = Just 7
mPrec Eq         = Just 4
mPrec Neq        = Just 4
mPrec Geq        = Just 4
mPrec Gt         = Just 4
mPrec Lt         = Just 4
mPrec Leq        = Just 4
mPrec Matches    = Just 5
mPrec NotMatches = Just 5
mPrec MMatch     = Just 5
mPrec Report     = Just 4
mPrec And        = Just 3
mPrec Or         = Just 2
mPrec _          = Nothing

instance PS (E a) where
    ps _ (Column _ i)    = "$" <> pretty i
    ps _ AllColumn{}     = "$0"
    ps _ IParseAllCol{}  = "$0:i"
    ps _ FParseAllCol{}  = "$0:f"
    ps _ ParseAllCol{}   = "$0:"
    ps _ (IParseCol _ i) = "$" <> pretty i <> ":i"
    ps _ (FParseCol _ i) = "$" <> pretty i <> ":f"
    ps _ (ParseCol _ i)  = "$" <> pretty i <> ":"
    ps _ AllField{}      = "`0"
    ps _ (Field _ i)     = "`" <> pretty i
    ps _ LastField{}     = "`*"
    ps _ FieldList{}     = "`$"
    ps d (EApp _ (EApp _ (BB _ op) e0) e1) | Just d' <- mPrec op = parensp (d>d') (ps (d'+1) e0 <+> pretty op <+> ps (d'+1) e1)
    ps _ (EApp _ (BB _ op) e) | isJust (mPrec op) = parens (pretty e <+> pretty op)
    ps d (EApp _ (UB _ (At i)) e)     = parensp (d>0) (ps 1 e) <> "." <> pretty i
    ps d (EApp _ (UB _ (Select i)) e) = parensp (d>0) (ps 1 e) <> "->" <> pretty i
    ps d (EApp _ (UB _ (SelR n)) e)   = parensp (d>0) (ps 1 e) <> "->" <> pretty n
    ps _ (EApp _ (UB _ IParse) e')    = pretty e' <> ":i"
    ps _ (EApp _ (UB _ FParse) e')    = pretty e' <> ":f"
    ps _ (EApp _ (UB _ Parse) e')     = pretty e' <> ":"
    ps d (EApp _ (EApp _ (EApp _ (TB _ Bookend) e) e') e'')  = parensp (d>3) (ps 4 e <> ",," <> ps 4 e' <+> ps 5 e'')
    ps d (EApp _ (EApp _ (EApp _ (TB _ Fold) e) e') e'')     = parensp (d>5) (ps 6 e <> "|" <> ps 6 e' <+> ps 7 e'')
    ps d (EApp _ (EApp _ (EApp _ (TB _ Scan) e) e') e'')     = parensp (d>5) (ps 6 e <> "^" <> ps 6 e' <+> ps 7 e'')
    ps d (EApp _ (EApp _ (EApp _ (TB _ ZipW) op) e') e'')    = parensp (d>5) ("," <> ps 6 op <+> ps 7 e' <+> ps 8 e'')
    ps d (EApp _ (EApp _ (EApp _ (TB _ Captures) e) e') e'') = parensp (d>6) (ps 7 e <+> "~*" <+> ps 7 e' <+> ps 8 e'')
    ps d (EApp _ e0 e1) = parensp (d>10) (ps 10 e0 <+> ps 11 e1)
    ps d (OptionVal _ (Just e)) = parensp (d>10) ("Some" <+> ps 11 e)
    ps _ (OptionVal _ Nothing) = "None"
    ps _ (BB _ op)         = parens (pretty op)
    ps _ (Lit _ l)         = pretty l
    ps _ (Var _ n)         = pretty n
    ps _ (F n)             = pretty n
    ps _ (RegexLit _ rr)   = "/" <> pretty (decodeUtf8 rr) <> "/"
    ps _ (UB _ u)          = pretty u
    ps _ (ResVar _ x)      = pretty x
    ps _ (Dfn _ e)         = brackets (pretty e)
    ps _ (NB _ n)          = pretty n
    ps _ RC{}              = "(compiled regex)"
    ps _ (Guarded _ p e)   = braces (pretty p) <> braces (pretty e)
    ps _ (Implicit _ e)    = braces ("|" <+> pretty e)
    ps _ (Paren _ e)       = parens (pretty e)
    ps _ (RwB _ MapMaybe)  = "mapMaybe"
    ps _ (RwB _ DedupOn)   = "dedupOn"
    ps _ (RwB _ Filter)    = "filter"
    ps _ (RwT _ Fold)      = "fold"
    ps _ (RwT _ Scan)      = "scan"
    ps _ (RwB _ Fold1)     = "fold1"
    ps _ (Cond _ e0 e1 e2) = "?" <> pretty e0 <> ";" <+> pretty e1 <> ";" <+> pretty e2
    ps _ (Tup _ es)        = j'Tup es
    ps _ (Rec _ es)        = jrec ((\(n,e) -> pretty n <+> ".=" <+> pretty e)<$>es)
    ps _ (Arr _ es)        = tupledByFunky "," (V.toList $ pretty <$> es)
    ps _ (Anchor _ es)     = "&" <> tupledBy "." (pretty <$> es)
    ps _ (Let _ (n, b) e)  = "let" <+> "val" <+> pretty n <+> ":=" <+> pretty b <+> "in" <+> pretty e <+> "end"
    ps d (Lam _ n e)       = parensp (d>1) ("λ" <> pretty n <> "." <+> ps 2 e)
    ps _ (TB _ g)          = pretty g

instance Pretty (E a) where pretty=ps 0

instance Show (E a) where show=show.pretty

-- for tests
instance Eq (E a) where
    (==) (Column _ i) (Column _ j)              = i == j
    (==) (IParseCol _ i) (IParseCol _ j)        = i == j
    (==) (FParseCol _ i) (FParseCol _ j)        = i == j
    (==) (Field _ i) (Field _ j)                = i == j
    (==) LastField{} LastField{}                = True
    (==) FieldList{} FieldList{}                = True
    (==) AllColumn{} AllColumn{}                = True
    (==) AllField{} AllField{}                  = True
    (==) (EApp _ e0 e1) (EApp _ e0' e1')        = e0 == e0' && e1 == e1'
    (==) (Guarded _ p e) (Guarded _ p' e')      = p == p' && e == e'
    (==) (Implicit _ e) (Implicit _ e')         = e == e'
    (==) (Let _ (n, eϵ) e) (Let _ (n', eϵ') e') = eqName n n' && e == e' && eϵ == eϵ'
    (==) (Var _ n) (Var _ n')                   = eqName n n'
    (==) (Lam _ n e) (Lam _ n' e')              = eqName n n' && e == e'
    (==) (Lit _ l) (Lit _ l')                   = l == l'
    (==) (RegexLit _ rr) (RegexLit _ rr')       = rr == rr'
    (==) (BB _ b) (BB _ b')                     = b == b'
    (==) (TB _ b) (TB _ b')                     = b == b'
    (==) (UB _ unOp) (UB _ unOp')               = unOp == unOp'
    (==) (NB _ x) (NB _ y)                      = x == y
    (==) (Tup _ es) (Tup _ es')                 = es == es'
    (==) (Rec _ es) (Rec _ es')                 = es == es'
    (==) (ResVar _ x) (ResVar _ y)              = x == y
    (==) (Dfn _ f) (Dfn _ g)                    = f == g -- we're testing for lexical equivalence
    (==) RC{} _                                 = error "Cannot compare compiled regex!"
    (==) _ RC{}                                 = error "Cannot compare compiled regex!"
    (==) (Paren _ e) e'                         = e == e'
    (==) e (Paren _ e')                         = e == e'
    (==) (RwB _ b) (RwB _ b')                   = b == b'
    (==) (RwT _ b) (RwT _ b')                   = b == b'
    (==) _ _                                    = False

data C = IsNum | IsEq | IsOrd
       | IsParse | IsPrintf
       | IsSemigroup | IsMonoid
       | Functor -- ^ For map (@"@)
       | Foldable | Witherable
       deriving (Eq, Ord)

instance Pretty C where
    pretty IsNum = "Num"; pretty IsEq = "Eq"; pretty IsOrd = "Ord"
    pretty IsParse = "Parseable"; pretty IsSemigroup = "Semigroup"
    pretty Functor = "Functor"; pretty Foldable = "Foldable"
    pretty IsPrintf = "Printf"; pretty Witherable = "Witherable"
    pretty IsMonoid = "Monoid"

instance Show C where show=show.pretty

-- decl
data D a = SetFS T.Text | SetRS T.Text
         | FunDecl (Nm a) [Nm a] (E a)
         | FlushDecl
         | SetAsv | SetUsv | SetCsv
         | SetOFS T.Text | SetORS T.Text
         deriving (Functor)

instance Pretty (D a) where
    pretty (SetFS bs)       = ":set fs :=" <+> "/" <> pretty bs <> "/;"
    pretty (SetRS rs)       = ":set rs :=" <+> "/" <> pretty rs <> "/;"
    pretty (FunDecl n ns e) = "fn" <+> pretty n <> tupled (pretty <$> ns) <+> ":=" <#> indent 2 (pretty e <> ";")
    pretty FlushDecl        = ":flush;"
    pretty SetAsv           = ":set asv;"
    pretty SetUsv           = ":set usv;"
    pretty SetCsv           = ":set csv;"
    pretty (SetOFS sep)     = ":set ofs :=" <+> "'" <> pretty sep <> "';"
    pretty (SetORS sep)     = ":set ors :=" <+> "'" <> pretty sep <> "';"

data Program a = Program { decls :: [D a], expr :: E a } deriving (Functor)

instance Pretty (Program a) where
    pretty (Program ds e) = concatWith (<##>) (pretty <$> ds) <##> pretty e

instance Show (Program a) where show=show.pretty

flushD :: Program a -> Bool
flushD (Program ds _) = any p ds where p FlushDecl = True; p _ = False

data Mode = CSV | AWK (Maybe T.Text) (Maybe T.Text) -- field, record

getS :: Program a -> Mode
getS (Program ds _) = foldl' go (AWK Nothing Nothing) ds where
    go (AWK _ rs) (SetFS bs) = AWK (Just bs) rs
    go _ SetAsv              = AWK (Just "\\x1f") (Just "\\x1e")
    go _ SetUsv              = AWK (Just "␞") (Just "␟")
    go _ SetCsv              = CSV
    go (AWK fs _) (SetRS bs) = AWK fs (Just bs)
    go next _                = next

mapExpr :: (E a -> E a) -> Program a -> Program a
mapExpr f (Program ds e) = Program ds (f e)
