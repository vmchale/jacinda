{
    {-# LANGUAGE OverloadedStrings #-}
    module L ( alexMonadScan
             , alexInitUserState
             , runAlexSt
             , withAlexSt
             , freshName
             , newVarAlex
             , AlexPosn (..)
             , Alex (..)
             , Token (..)
             , Keyword (..)
             , Sym (..)
             , Builtin (..)
             , Var (..)
             , AlexUserState
             ) where

import Control.Arrow ((&&&))
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor (($>))
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Text as T
import Nm
import Prettyprinter (Pretty (pretty), (<+>), colon, squotes)
import U

}

%wrapper "monadUserState-strict-text"

$digit = [0-9]

$latin = [a-zA-Z]

@follow_char = [$latin $digit \_]

$str_special = [\\\']

@escape_str = \\ ([$str_special nt] | "ESC")

@string = \' ([^ $str_special] | @escape_str)* \'

@escape_rr = \\ [\\\/]

@rr = "/" ([^\/] | @escape_rr)* "/"

@name = [a-z] @follow_char*
@tyname = [A-Z] @follow_char*

@float = $digit+\.$digit+

tokens :-

    <dfn> {
        x                        { res VarX }
        y                        { res VarY }
    }

    <0> "["                      { sym LSqBracket `andBegin` dfn } -- FIXME: this doesn't allow nested

    <0,dfn> {

        $white+                  ;

        "{.".*                   ;
        "#!".*                   ; -- shebang

        ":="                     { sym DefEq }
        "≔"                      { sym DefEq }
        "{"                      { sym LBrace }
        "}"                      { sym RBrace }

        "#."                     { sym FilterTok }

        -- symbols/operators
        "%"                      { sym PercentTok }
        "*"                      { sym TimesTok }
        "**"                     { sym ExpTok }
        "+"                      { sym PlusTok }
        "-"                      { sym MinusTok }

        "|"                      { sym FoldTok }
        \"                       { sym Quot }
        "^"                      { sym Caret }
        "^*"                     { sym CaretStar }
        "|>"                     { sym Fold1Tok }
        ⍬                        { sym Zilde }

        "="                      { sym EqTok }
        "!="                     { sym NeqTok }
        "<="                     { sym LeqTok }
        "<"                      { sym LtTok }
        ">="                     { sym GeqTok }
        ">"                      { sym GtTok }
        "&"                      { sym AndTok }
        "||"                     { sym OrTok }
        "("                      { sym LParen }
        ")"                      { sym RParen }
        "&("                     { sym LAnchor }
        "$>"                     { sym IceCreamCone }
        "{%"                     { sym LBracePercent }
        "#{"                     { sym LBraceOctothorpe }
        "{|"                     { sym LBraceBar }
        "]"                      { sym RSqBracket `andBegin` 0 }
        ".="                     { sym DotEq }
        "~"                      { sym Tilde }
        "!~"                     { sym NotMatchTok }
        "~?"                     { sym MMatch }
        ","                      { sym Comma }
        ",,"                     { sym DoubleComma }
        "."                      { sym Dot }
        "#"                      { sym TallyTok }
        "#*"                     { sym LengthTok }
        "[:"                     { sym ConstTok }
        "!"                      { sym Exclamation }
        ":"                      { sym Colon }
        ";"                      { sym Semicolon }
        "\."                     { sym BackslashDot }
        \\                       { sym Backslash }
        λ                        { sym Backslash }
        "|`"                     { sym CeilSym }
        ⌈                        { sym CeilSym }
        "|."                     { sym FloorSym }
        ⌊                        { sym FloorSym }
        "~."                     { sym DedupTok }
        dedup                    { sym DedupTok }
        "~.*"                    { sym DedupOnTok }
        ".?"                     { sym CatMaybesTok }
        catMaybes                { sym CatMaybesTok }
        ":?"                     { sym MapMaybeTok }
        "~*"                     { sym CapTok }
        "-."                     { sym NegTok }
        "`*"                     { sym LastFieldTok }
        "`$"                     { sym FieldListTok }
        \?                       { sym QuestionMark }
        "@@"                     { sym AmpAmp }

        in                       { mkKw KwIn }
        let                      { mkKw KwLet }
        val                      { mkKw KwVal }
        end                      { mkKw KwEnd }
        :set                     { mkKw KwSet }
        :flush                   { mkKw KwFlush }
        fn                       { mkKw KwFn }
        "@include"               { mkKw KwInclude }
        if                       { mkKw KwIf }
        then                     { mkKw KwThen }
        else                     { mkKw KwElse }
        asv                      { mkKw KwAsv }
        usv                      { mkKw KwUsv }
        csv                      { mkKw KwCsv }

        fs                       { res VarFs }
        rs                       { res VarRs }
        ix                       { res VarIx }
        ⍳                        { res VarIx }
        nf                       { res VarNf }
        ¨                        { sym Quot }
        min                      { res VarMin }
        max                      { res VarMax }

        substr                   { mkBuiltin BSubstr }
        split                    { mkBuiltin BSplit }
        splitc                   { mkBuiltin BSplitc }
        sprintf                  { mkBuiltin BSprintf }
        option                   { mkBuiltin BOption }
        floor                    { mkBuiltin BFloor }
        ceil                     { mkBuiltin BCeil }
        match                    { mkBuiltin BMatch }
        ices                     { mkBuiltin BIxes }
        captures                 { mkBuiltin BCaptures }
        Some                     { mkBuiltin BSome }
        None                     { mkBuiltin BNone }
        fp                       { mkBuiltin BFp }
        mapMaybe                 { mkBuiltin BMapMaybe }
        dedupOn                  { mkBuiltin BDedupOn }
        filter                   { mkBuiltin BFilt }
        fold                     { mkBuiltin BFold }
        fold1                    { mkBuiltin BFold1 }
        scan                     { mkBuiltin BScan }
        "sub1"                   { mkBuiltin BSub1 }
        subs                     { mkBuiltin BSubs }
        "head#"                  { mkBuiltin BHead }
        "tail#"                  { mkBuiltin BTail }
        "last#"                  { mkBuiltin BLast }
        "init#"                  { mkBuiltin BInit }
        "take#"                  { mkBuiltin BTake }
        "drop#"                  { mkBuiltin BDrop }
        reintercalate            { mkBuiltin BRein }

        ":i"                     { mkBuiltin BIParse }
        ":f"                     { mkBuiltin BFParse }

        "#t"                     { tok (\p _ -> alex $ TokBool p True) }
        "#f"                     { tok (\p _ -> alex $ TokBool p False) }

        \$$digit+                { tok (\p s -> alex $ TokStreamLit p (read $ T.unpack $ T.tail s)) }
        `$digit+                 { tok (\p s -> alex $ TokFieldLit p (read $ T.unpack $ T.tail s)) }

        "."$digit+               { tok (\p s -> alex $ TokAccess p (read $ T.unpack $ T.tail s)) }
        "->"$digit+              { tok (\p s -> alex $ TokSelect p (read $ T.unpack $ T.drop 2 s)) }
        "->"$latin+              { tok (\p s -> TokR p <$> newIdentAlex p (T.drop 2 s)) }
        $digit+                  { tok (\p s -> alex $ TokInt p (read $ T.unpack s)) }
        _$digit+                 { tok (\p s -> alex $ TokInt p (negate $ read $ T.unpack $ T.tail s)) }

        $digit+\.$digit+         { tok (\p s -> alex $ TokFloat p (read $ T.unpack s)) }
        _$digit+\.$digit+        { tok (\p s -> alex $ TokFloat p (negate $ read $ T.unpack $ T.tail s)) }

        @string                  { tok (\p s -> alex $ TokStr p (escReplace $ T.init $ T.tail s)) }

        -- TODO: allow chars to be escaped
        @rr                      { tok (\p s -> alex $ TokRR p (escRr $ T.init $ T.tail s)) }

        @name                    { tok (\p s -> TokName p <$> newIdentAlex p s) }
        @tyname                  { tok (\p s -> TokTyName p <$> newIdentAlex p s) }

    }

{

dropQuotes :: BSL.ByteString -> BSL.ByteString
dropQuotes = BSL.init . BSL.tail

alex :: a -> Alex a
alex = pure

tok f (p,_,_,s) len = f p (T.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

res = constructor TokResVar

mkKw = constructor TokKeyword

sym = constructor TokSym

mkBuiltin = constructor TokBuiltin

-- this is inefficient but w/e
escReplace :: T.Text -> T.Text
escReplace =
      T.replace "\\\"" "\""
    . T.replace "\\\\" "\\"
    . T.replace "\\ESC" "\ESC"
    . T.replace "\\n" "\n"
    . T.replace "\\t" "\t"

escRr :: T.Text -> T.Text
escRr = T.replace "\\/" "/"

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> colon <> pretty col

-- functional bimap?
type AlexUserState = (Int, M.Map T.Text Int, IM.IntMap (Nm AlexPosn))

alexInitUserState :: AlexUserState
alexInitUserState = (0, mempty, mempty)

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

alexEOF = EOF <$> get_pos

data Sym = PlusTok | MinusTok | PercentTok
         | ExpTok
         | FoldTok
         | IceCreamCone
         | Fold1Tok
         | Quot
         | TimesTok
         | DefEq
         | Colon
         | LBrace | RBrace
         | LParen
         | LAnchor
         | DotEq
         | RParen
         | LSqBracket | RSqBracket
         | Semicolon
         | Underscore
         | EqTok | NeqTok
         | GtTok | GeqTok
         | LtTok | LeqTok
         | AndTok | OrTok
         | Tilde | NotMatchTok | MMatch
         | Comma | Dot
         | DoubleComma
         | TallyTok | LengthTok
         | ConstTok
         | LBracePercent
         | LBraceOctothorpe
         | LBraceBar
         | Exclamation
         | Caret | CaretStar
         | Zilde
         | Backslash
         | BackslashDot
         | QuestionMark
         | FilterTok
         | FloorSym | CeilSym
         | DedupTok | DedupOnTok
         | CatMaybesTok | MapMaybeTok
         | CapTok
         | NegTok
         | LastFieldTok | FieldListTok
         | AmpAmp

instance Pretty Sym where
    pretty PlusTok          = "+"
    pretty MinusTok         = "-"
    pretty PercentTok       = "%"
    pretty ExpTok           = "**"
    pretty FoldTok          = "|"
    pretty Fold1Tok         = "|>"
    pretty TimesTok         = "*"
    pretty DefEq            = ":="
    pretty DotEq            = ".="
    pretty Colon            = ":"
    pretty LBrace           = "{"
    pretty RBrace           = "}"
    pretty Semicolon        = ";"
    pretty Underscore       = "_"
    pretty EqTok            = "="
    pretty LeqTok           = "<="
    pretty LtTok            = "<"
    pretty NeqTok           = "!="
    pretty GeqTok           = ">="
    pretty GtTok            = ">"
    pretty AndTok           = "&"
    pretty OrTok            = "||"
    pretty LParen           = "("
    pretty RParen           = ")"
    pretty LAnchor          = "&("
    pretty LSqBracket       = "["
    pretty RSqBracket       = "]"
    pretty Tilde            = "~"
    pretty NotMatchTok      = "!~"
    pretty MMatch           = "~?"
    pretty Comma            = ","
    pretty DoubleComma      = ",,"
    pretty Dot              = "."
    pretty TallyTok         = "#"
    pretty Zilde            = "⍬"
    pretty LengthTok        = "#*"
    pretty Quot             = "¨"
    pretty Caret            = "^"
    pretty CaretStar        = "^*"
    pretty ConstTok         = "[:"
    pretty LBracePercent    = "{%"
    pretty LBraceOctothorpe = "#{"
    pretty LBraceBar        = "{|"
    pretty Exclamation      = "!"
    pretty Backslash        = "\\"
    pretty BackslashDot     = "\\."
    pretty FilterTok        = "#."
    pretty FloorSym         = "⌊"
    pretty CeilSym          = "⌈"
    pretty DedupTok         = "~."
    pretty DedupOnTok       = "~.*"
    pretty CatMaybesTok     = ".?"
    pretty MapMaybeTok      = ":?"
    pretty CapTok           = "~*"
    pretty NegTok           = "-."
    pretty LastFieldTok     = "`*"
    pretty FieldListTok     = "`$"
    pretty IceCreamCone     = "$>"
    pretty QuestionMark     = "?"
    pretty AmpAmp           = "@@"

data Keyword = KwLet
             | KwIn
             | KwVal
             | KwEnd
             | KwSet
             | KwFlush
             | KwFn
             | KwInclude
             | KwIf | KwThen | KwElse
             | KwAsv | KwUsv | KwCsv

-- | Reserved/special variables
data Var = VarX | VarY
         | VarFs | VarRs
         | VarOfs | VarOrs
         | VarIx | VarNf
         | VarMin | VarMax

instance Pretty Var where
    pretty VarX = "x"; pretty VarY = "y"
    pretty VarFs = "fs"; pretty VarRs = "rs"
    pretty VarOrs = "ors"; pretty VarOfs = "ofs"
    pretty VarMin = "min"; pretty VarMax = "max"
    pretty VarIx = "⍳"; pretty VarNf = "nf"

instance Pretty Keyword where
    pretty KwLet     = "let"
    pretty KwIn      = "in"
    pretty KwVal     = "val"
    pretty KwEnd     = "end"
    pretty KwFn      = "fn"
    pretty KwSet     = ":set"
    pretty KwFlush   = ":flush"
    pretty KwInclude = "@include"
    pretty KwIf      = "if"
    pretty KwThen    = "then"
    pretty KwElse    = "else"
    pretty KwUsv     = "usv"
    pretty KwAsv     = "asv"
    pretty KwCsv     = "csv"

data Builtin = BIParse | BFParse
             | BSubstr
             | BSplit | BSplitc
             | BOption
             | BSprintf
             | BFloor | BCeil
             | BMatch | BIxes
             | BCaptures
             | BSome | BNone
             | BFp
             | BMapMaybe
             | BDedupOn
             | BFilt
             | BFold | BFold1
             | BScan
             | BSub1 | BSubs
             | BHead | BTail
             | BInit | BLast
             | BDrop | BTake
             | BRein

instance Pretty Builtin where
    pretty BIParse   = ":i"
    pretty BFParse   = ":f"
    pretty BSubstr   = "substr"
    pretty BSplit    = "split"
    pretty BOption   = "option"
    pretty BSplitc   = "splitc"
    pretty BSprintf  = "sprintf"
    pretty BFloor    = "floor"
    pretty BCeil     = "ceil"
    pretty BMatch    = "match"
    pretty BIxes     = "ices"
    pretty BSome     = "Some"
    pretty BNone     = "None"
    pretty BFp       = "fp"
    pretty BCaptures = "captures"
    pretty BMapMaybe = "mapMaybe"
    pretty BDedupOn  = "dedupOn"
    pretty BFilt     = "filter"
    pretty BFold     = "fold"
    pretty BFold1    = "fold1"
    pretty BScan     = "scan"
    pretty BSub1     = "sub1"
    pretty BSubs     = "subs"
    pretty BHead     = "head#"
    pretty BTail     = "tail#"
    pretty BInit     = "init#"
    pretty BLast     = "last#"
    pretty BTake     = "take#"
    pretty BDrop     = "drop#"
    pretty BRein     = "reintercalate"

data Token a = EOF { loc :: a }
             | TokSym { loc :: a, _sym :: Sym }
             | TokName { loc :: a, _name :: Nm a }
             | TokTyName { loc :: a, _tyName :: TyName a }
             | TokBuiltin { loc :: a, _builtin :: Builtin }
             | TokKeyword { loc :: a, _kw :: Keyword }
             | TokResVar { loc :: a, _var :: Var }
             | TokInt { loc :: a, int :: Integer }
             | TokFloat { loc :: a, float :: Double }
             | TokBool { loc :: a, boolTok :: Bool }
             | TokStr { loc :: a, strTok :: T.Text }
             | TokStreamLit { loc :: a, ix :: Int }
             | TokFieldLit { loc :: a, ix :: Int }
             | TokRR { loc :: a, rr :: T.Text }
             | TokAccess { loc :: a, ix :: Int }
             | TokSelect { loc :: a, field :: Int }
             | TokR { loc :: a, nfield :: Nm a }

instance Pretty (Token a) where
    pretty EOF{}              = "(eof)"
    pretty (TokSym _ s)       = "symbol" <+> squotes (pretty s)
    pretty (TokName _ n)      = "identifier" <+> squotes (pretty n)
    pretty (TokTyName _ tn)   = "identifier" <+> squotes (pretty tn)
    pretty (TokBuiltin _ b)   = "builtin" <+> squotes (pretty b)
    pretty (TokKeyword _ kw)  = "keyword" <+> squotes (pretty kw)
    pretty (TokInt _ i)       = pretty i
    pretty (TokStr _ str)     = squotes (pretty str)
    pretty (TokStreamLit _ i) = "$" <> pretty i
    pretty (TokFieldLit _ i)  = "`" <> pretty i
    pretty (TokRR _ rr')      = "/" <> pretty rr' <> "/"
    pretty (TokResVar _ v)    = "reserved variable" <+> squotes (pretty v)
    pretty (TokBool _ True)   = "#t"
    pretty (TokBool _ False)  = "#f"
    pretty (TokAccess _ i)    = "." <> pretty i
    pretty (TokFloat _ f)     = pretty f
    pretty (TokSelect _ i)    = "->" <> pretty i
    pretty (TokR _ r)         = "->" <> pretty r

freshName :: T.Text -> Alex (Nm AlexPosn)
freshName t = do
    pos <- get_pos
    (i, ns, us) <- alexGetUserState
    let (j, n) = freshIdent pos t i
    alexSetUserState (j, ns, us) $> n

newVarAlex :: T.Text -> Alex (Nm AlexPosn)
newVarAlex t = do {pos <- get_pos; newIdentAlex pos t}

newIdentAlex :: AlexPosn -> T.Text -> Alex (Nm AlexPosn)
newIdentAlex pos t = do
    st <- alexGetUserState
    let (st', n) = newIdent pos t st
    alexSetUserState st' $> n

freshIdent :: AlexPosn -> T.Text -> Int -> (Int, Nm AlexPosn)
freshIdent pos t max' =
    let i=max'+1; nm=Nm t (U i) pos
        in (i, nm)

newIdent :: AlexPosn -> T.Text -> AlexUserState -> (AlexUserState, Nm AlexPosn)
newIdent pos t pre@(max', names, uniqs) =
    case M.lookup t names of
        Just i -> (pre, Nm t (U i) pos)
        Nothing -> let i = max' + 1
            in let newName = Nm t (U i) pos
                in ((i, M.insert t i names, IM.insert i newName uniqs), newName)

runAlexSt :: T.Text -> Alex a -> Either String (AlexUserState, a)
runAlexSt inp = withAlexSt inp alexInitUserState

withAlexSt :: T.Text -> AlexUserState -> Alex a -> Either String (AlexUserState, a)
withAlexSt inp ust (Alex f) = first alex_ust <$> f
    (AlexState { alex_bytes = []
               , alex_pos = alexStartPos
               , alex_inp = inp
               , alex_chr = '\n'
               , alex_ust = ust
               , alex_scd = 0
               })

}
