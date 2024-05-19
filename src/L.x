{
    {-# LANGUAGE OverloadedStrings #-}
    module L ( alexMonadScan
             , alexInitUserState
             , runAlex
             , runAlexSt
             , withAlexSt
             , freshName
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

@escape_str = \\ [$str_special nt]

@string = \' ([^ $str_special] | @escape_str)* \'

@escape_rr = \\ [\\\/]

@rr = "/" ([^\/] | @escape_rr)* "/"

@name = [a-z] @follow_char*
@tyname = [A-Z] @follow_char*

@float = $digit+\.$digit+

tokens :-

    <dfn> {
        x                        { mkRes VarX }
        y                        { mkRes VarY }
    }

    <0> "["                      { mkSym LSqBracket `andBegin` dfn } -- FIXME: this doesn't allow nested

    <0,dfn> {

        $white+                  ;

        "{.".*                   ;
        "#!".*                   ; -- shebang

        ":="                     { mkSym DefEq }
        "≔"                      { mkSym DefEq }
        "{"                      { mkSym LBrace }
        "}"                      { mkSym RBrace }

        "#."                     { mkSym FilterTok }

        -- symbols/operators
        "%"                      { mkSym PercentTok }
        "*"                      { mkSym TimesTok }
        "**"                     { mkSym ExpTok }
        "+"                      { mkSym PlusTok }
        "-"                      { mkSym MinusTok }

        "|"                      { mkSym FoldTok }
        \"                       { mkSym Quot }
        "^"                      { mkSym Caret }
        "|>"                     { mkSym Fold1Tok }
        ⍬                        { mkSym Zilde }

        "="                      { mkSym EqTok }
        "!="                     { mkSym NeqTok }
        "<="                     { mkSym LeqTok }
        "<"                      { mkSym LtTok }
        ">="                     { mkSym GeqTok }
        ">"                      { mkSym GtTok }
        "&"                      { mkSym AndTok }
        "||"                     { mkSym OrTok }
        "("                      { mkSym LParen }
        ")"                      { mkSym RParen }
        "&("                     { mkSym LAnchor }
        "$>"                     { mkSym IceCreamCone }
        "{%"                     { mkSym LBracePercent }
        "{|"                     { mkSym LBraceBar }
        "]"                      { mkSym RSqBracket `andBegin` 0 }
        "~"                      { mkSym Tilde }
        "!~"                     { mkSym NotMatchTok }
        ","                      { mkSym Comma }
        ",,"                     { mkSym DoubleComma }
        "."                      { mkSym Dot }
        "#"                      { mkSym TallyTok }
        "#*"                     { mkSym LengthTok }
        "[:"                     { mkSym ConstTok }
        "!"                      { mkSym Exclamation }
        ":"                      { mkSym Colon }
        ";"                      { mkSym Semicolon }
        "\."                     { mkSym BackslashDot }
        \\                       { mkSym Backslash }
        λ                        { mkSym Backslash }
        "|`"                     { mkSym CeilSym }
        ⌈                        { mkSym CeilSym }
        "|."                     { mkSym FloorSym }
        ⌊                        { mkSym FloorSym }
        "~."                     { mkSym DedupTok }
        dedup                    { mkSym DedupTok }
        "~.*"                    { mkSym DedupOnTok }
        ".?"                     { mkSym CatMaybesTok }
        catMaybes                { mkSym CatMaybesTok }
        ":?"                     { mkSym MapMaybeTok }
        "~*"                     { mkSym CapTok }
        "-."                     { mkSym NegTok }
        "`*"                     { mkSym LastFieldTok }
        "`$"                     { mkSym FieldListTok }
        \?                       { mkSym QuestionMark }

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

        fs                       { mkRes VarFs }
        rs                       { mkRes VarRs }
        ix                       { mkRes VarIx }
        ⍳                        { mkRes VarIx }
        nf                       { mkRes VarNf }
        ¨                        { mkSym Quot }
        min                      { mkRes VarMin }
        max                      { mkRes VarMax }

        substr                   { mkBuiltin BuiltinSubstr }
        split                    { mkBuiltin BuiltinSplit }
        splitc                   { mkBuiltin BuiltinSplitc }
        sprintf                  { mkBuiltin BuiltinSprintf }
        option                   { mkBuiltin BuiltinOption }
        floor                    { mkBuiltin BuiltinFloor }
        ceil                     { mkBuiltin BuiltinCeil }
        match                    { mkBuiltin BuiltinMatch }
        captures                 { mkBuiltin BuiltinCaptures }
        Some                     { mkBuiltin BuiltinSome }
        None                     { mkBuiltin BuiltinNone }
        fp                       { mkBuiltin BuiltinFp }
        mapMaybe                 { mkBuiltin BuiltinMapMaybe }
        dedupOn                  { mkBuiltin BuiltinDedupOn }
        filter                   { mkBuiltin BuiltinFilt }
        fold                     { mkBuiltin BuiltinFold }
        fold1                    { mkBuiltin BuiltinFold1 }
        scan                     { mkBuiltin BuiltinScan }
        "sub1"                   { mkBuiltin BuiltinSub1 }
        subs                     { mkBuiltin BuiltinSubs }
        "head#"                  { mkBuiltin BuiltinHead }
        "tail#"                  { mkBuiltin BuiltinTail }
        "last#"                  { mkBuiltin BuiltinLast }
        "init#"                  { mkBuiltin BuiltinInit }
        "take#"                  { mkBuiltin BuiltinTake }
        "drop#"                  { mkBuiltin BuiltinDrop }

        ":i"                     { mkBuiltin BuiltinIParse }
        ":f"                     { mkBuiltin BuiltinFParse }

        "#t"                     { tok (\p _ -> alex $ TokBool p True) }
        "#f"                     { tok (\p _ -> alex $ TokBool p False) }

        \$$digit+                { tok (\p s -> alex $ TokStreamLit p (read $ T.unpack $ T.tail s)) }
        `$digit+                 { tok (\p s -> alex $ TokFieldLit p (read $ T.unpack $ T.tail s)) }

        "."$digit+               { tok (\p s -> alex $ TokAccess p (read $ T.unpack $ T.tail s)) }
        "->"$digit+              { tok (\p s -> alex $ TokSelect p (read $ T.unpack $ T.drop 2 s)) }
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

mkRes = constructor TokResVar

mkKw = constructor TokKeyword

mkSym = constructor TokSym

mkBuiltin = constructor TokBuiltin

-- this is inefficient but w/e
escReplace :: T.Text -> T.Text
escReplace =
      T.replace "\\\"" "\""
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

data Sym = PlusTok
         | MinusTok
         | PercentTok
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
         | RParen
         | LSqBracket | RSqBracket
         | Semicolon
         | Underscore
         | EqTok | NeqTok
         | GtTok | GeqTok
         | LtTok | LeqTok
         | AndTok | OrTok
         | Tilde | NotMatchTok
         | Comma
         | DoubleComma
         | Dot
         | TallyTok
         | LengthTok
         | ConstTok
         | LBracePercent
         | LBraceBar
         | Exclamation
         | Caret
         | Zilde
         | Backslash
         | BackslashDot
         | QuestionMark
         | FilterTok
         | FloorSym
         | CeilSym
         | DedupTok
         | DedupOnTok
         | CatMaybesTok
         | MapMaybeTok
         | CapTok
         | NegTok
         | LastFieldTok
         | FieldListTok

instance Pretty Sym where
    pretty PlusTok       = "+"
    pretty MinusTok      = "-"
    pretty PercentTok    = "%"
    pretty ExpTok        = "**"
    pretty FoldTok       = "|"
    pretty Fold1Tok      = "|>"
    pretty TimesTok      = "*"
    pretty DefEq         = ":="
    pretty Colon         = ":"
    pretty LBrace        = "{"
    pretty RBrace        = "}"
    pretty Semicolon     = ";"
    pretty Underscore    = "_"
    pretty EqTok         = "="
    pretty LeqTok        = "<="
    pretty LtTok         = "<"
    pretty NeqTok        = "!="
    pretty GeqTok        = ">="
    pretty GtTok         = ">"
    pretty AndTok        = "&"
    pretty OrTok         = "||"
    pretty LParen        = "("
    pretty RParen        = ")"
    pretty LAnchor       = "&("
    pretty LSqBracket    = "["
    pretty RSqBracket    = "]"
    pretty Tilde         = "~"
    pretty NotMatchTok   = "!~"
    pretty Comma         = ","
    pretty DoubleComma   = ",,"
    pretty Dot           = "."
    pretty TallyTok      = "#"
    pretty Zilde         = "⍬"
    pretty LengthTok     = "#*"
    pretty Quot          = "¨"
    pretty Caret         = "^"
    pretty ConstTok      = "[:"
    pretty LBracePercent = "{%"
    pretty LBraceBar     = "{|"
    pretty Exclamation   = "!"
    pretty Backslash     = "\\"
    pretty BackslashDot  = "\\."
    pretty FilterTok     = "#."
    pretty FloorSym      = "⌊"
    pretty CeilSym       = "⌈"
    pretty DedupTok      = "~."
    pretty DedupOnTok    = "~.*"
    pretty CatMaybesTok  = ".?"
    pretty MapMaybeTok   = ":?"
    pretty CapTok        = "~*"
    pretty NegTok        = "-."
    pretty LastFieldTok  = "`*"
    pretty FieldListTok  = "`$"
    pretty IceCreamCone  = "$>"
    pretty QuestionMark  = "?"

data Keyword = KwLet
             | KwIn
             | KwVal
             | KwEnd
             | KwSet
             | KwFlush
             | KwFn
             | KwInclude
             | KwIf | KwThen | KwElse
             | KwAsv | KwUsv

-- | Reserved/special variables
data Var = VarX | VarY
         | VarFs | VarRs
         | VarOfs | VarOrs
         | VarIx | VarNf
         | VarMin | VarMax

instance Pretty Var where
    pretty VarX     = "x"
    pretty VarY     = "y"
    pretty VarFs    = "fs"
    pretty VarRs    = "rs"
    pretty VarOrs   = "ors"
    pretty VarOfs   = "ofs"
    pretty VarIx    = "⍳"
    pretty VarNf    = "nf"
    pretty VarMin   = "min"
    pretty VarMax   = "max"

instance Pretty Keyword where
    pretty KwLet     = "let"
    pretty KwIn      = "in"
    pretty KwVal     = "val"
    pretty KwEnd     = "end"
    pretty KwSet     = ":set"
    pretty KwFlush   = ":flush"
    pretty KwFn      = "fn"
    pretty KwInclude = "@include"
    pretty KwIf      = "if"
    pretty KwThen    = "then"
    pretty KwElse    = "else"
    pretty KwUsv     = "usv"
    pretty KwAsv     = "asv"

data Builtin = BuiltinIParse | BuiltinFParse
             | BuiltinSubstr
             | BuiltinSplit | BuiltinSplitc
             | BuiltinOption
             | BuiltinSprintf
             | BuiltinFloor | BuiltinCeil
             | BuiltinMatch
             | BuiltinCaptures
             | BuiltinSome | BuiltinNone
             | BuiltinFp
             | BuiltinMapMaybe
             | BuiltinDedupOn
             | BuiltinFilt
             | BuiltinFold | BuiltinFold1
             | BuiltinScan
             | BuiltinSub1 | BuiltinSubs
             | BuiltinHead | BuiltinTail
             | BuiltinInit | BuiltinLast
             | BuiltinDrop | BuiltinTake

instance Pretty Builtin where
    pretty BuiltinIParse   = ":i"
    pretty BuiltinFParse   = ":f"
    pretty BuiltinSubstr   = "substr"
    pretty BuiltinSplit    = "split"
    pretty BuiltinOption   = "option"
    pretty BuiltinSplitc   = "splitc"
    pretty BuiltinSprintf  = "sprintf"
    pretty BuiltinFloor    = "floor"
    pretty BuiltinCeil     = "ceil"
    pretty BuiltinMatch    = "match"
    pretty BuiltinSome     = "Some"
    pretty BuiltinNone     = "None"
    pretty BuiltinFp       = "fp"
    pretty BuiltinCaptures = "captures"
    pretty BuiltinMapMaybe = "mapMaybe"
    pretty BuiltinDedupOn  = "dedupOn"
    pretty BuiltinFilt     = "filter"
    pretty BuiltinFold     = "fold"
    pretty BuiltinFold1    = "fold1"
    pretty BuiltinScan     = "scan"
    pretty BuiltinSub1     = "sub1"
    pretty BuiltinSubs     = "subs"
    pretty BuiltinHead     = "head#"
    pretty BuiltinTail     = "tail#"
    pretty BuiltinInit     = "init#"
    pretty BuiltinLast     = "last#"
    pretty BuiltinTake     = "take#"
    pretty BuiltinDrop     = "drop#"

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

freshName :: T.Text -> Alex (Nm AlexPosn)
freshName t = do
    pos <- get_pos
    (i, ns, us) <- alexGetUserState
    let (j, n) = freshIdent pos t i
    alexSetUserState (j, ns, us) $> (n$>pos)

newIdentAlex :: AlexPosn -> T.Text -> Alex (Nm AlexPosn)
newIdentAlex pos t = do
    st <- alexGetUserState
    let (st', n) = newIdent pos t st
    alexSetUserState st' $> (n $> pos)

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
