{
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE StandaloneDeriving #-}
    module Jacinda.Lexer ( alexMonadScan
                         , alexInitUserState
                         , runAlex
                         , runAlexSt
                         , withAlexSt
                         , lexJac
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
import qualified Data.ByteString.Lazy.Char8 as ASCII
import Data.Functor (($>))
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Intern.Name
import Intern.Unique
import Prettyprinter (Pretty (pretty), (<+>), colon, squotes)

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

$latin = [a-zA-Z]

@follow_char = [$latin $digit \_]

@name = [a-z] @follow_char*
@tyname = [A-Z] @follow_char*

@float = $digit+\.$digit+

tokens :-

    <dfn> {
        x                        { mkRes VarX }
        y                        { mkRes VarY }
    }

    <0,dfn> {

        $white+                  ;

        "{.".*                   ;

        ":="                     { mkSym DefEq }
        "≔"                      { mkSym DefEq }
        "{"                      { mkSym LBrace }
        "}"                      { mkSym RBrace }

        "#."                     { mkSym FilterTok }

        -- symbols/operators
        "%"                      { mkSym PercentTok }
        "*"                      { mkSym TimesTok }
        "+"                      { mkSym PlusTok }
        "-"                      { mkSym MinusTok }

        "|"                      { mkSym FoldTok }
        \"                       { mkSym Quot }
        ¨                        { mkSym Quot }
        "^"                      { mkSym Caret }

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
        "{%"                     { mkSym LBracePercent }
        "{|"                     { mkSym LBraceBar }
        "["                      { mkSym LSqBracket `andBegin` dfn }
        "]"                      { mkSym RSqBracket `andBegin` 0 } -- FIXME: this doesn't allow nested
        "~"                      { mkSym Tilde }
        "!~"                     { mkSym NotMatchTok }
        ","                      { mkSym Comma }
        "."                      { mkSym Dot }
        "#"                      { mkSym TallyTok }
        "[:"                     { mkSym ConstTok }
        "!"                      { mkSym Exclamation }
        ":"                      { mkSym Colon }
        ";"                      { mkSym Semicolon }
        "\."                     { mkSym BackslashDot }
        \\                       { mkSym Backslash }

        in                       { mkKw KwIn }
        let                      { mkKw KwLet }
        val                      { mkKw KwVal }   
        end                      { mkKw KwEnd }
        :set                     { mkKw KwSet }
        fn                       { mkKw KwFn }

        fs                       { mkRes VarFs }
        ix                       { mkRes VarIx }
        ⍳                        { mkRes VarIx }
        min                      { mkRes VarMin }
        max                      { mkRes VarMax }

        substr                   { mkBuiltin BuiltinSubstr }
        split                    { mkBuiltin BuiltinSplit }
        splitc                   { mkBuiltin BuiltinSplitc }
        sprintf                  { mkBuiltin BuiltinSprintf }
        floor                    { mkBuiltin BuiltinFloor }
        ceil                     { mkBuiltin BuiltinCeil }

        ":i"                     { mkBuiltin BuiltinIParse }
        ":f"                     { mkBuiltin BuiltinFParse }

        "#t"                     { tok (\p _ -> alex $ TokBool p True) }
        "#f"                     { tok (\p _ -> alex $ TokBool p False) }
    
        \$$digit+                { tok (\p s -> alex $ TokStreamLit p (read $ ASCII.unpack $ BSL.tail s)) }
        `$digit+                 { tok (\p s -> alex $ TokFieldLit p (read $ ASCII.unpack $ BSL.tail s)) }

        "."$digit+               { tok (\p s -> alex $ TokAccess p (read $ ASCII.unpack $ ASCII.tail s)) }
        $digit+                  { tok (\p s -> alex $ TokInt p (read $ ASCII.unpack s)) }
        _$digit+                 { tok (\p s -> alex $ TokInt p (negate $ read $ ASCII.unpack $ BSL.tail s)) }

        $digit+\.$digit+         { tok (\p s -> alex $ TokFloat p (read $ ASCII.unpack s)) }
        _$digit+\.$digit+        { tok (\p s -> alex $ TokFloat p (negate $ read $ ASCII.unpack $ BSL.tail s)) }

        -- TODO: allow chars to be escaped
        -- TODO: consider dropping this syntax for strings?
        '[^']*'                  { tok (\p s -> alex $ TokStr p (BSL.init $ BSL.tail s)) }

        "/"[^\/]*"/"             { tok (\p s -> alex $ TokRR p (BSL.init $ BSL.tail s)) } -- TODO: allow slashes that are escaped

        @name                    { tok (\p s -> TokName p <$> newIdentAlex p (mkText s)) }
        @tyname                  { tok (\p s -> TokTyName p <$> newIdentAlex p (mkText s)) }

    }

{

dropQuotes :: BSL.ByteString -> BSL.ByteString
dropQuotes = BSL.init . BSL.tail

alex :: a -> Alex a
alex = pure

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkRes = constructor TokResVar

mkKw = constructor TokKeyword

mkSym = constructor TokSym

mkBuiltin = constructor TokBuiltin

mkText :: BSL.ByteString -> T.Text
mkText = decodeUtf8 . BSL.toStrict

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> colon <> pretty col

deriving instance Ord AlexPosn

-- functional bimap?
type AlexUserState = (Int, M.Map T.Text Int, IM.IntMap (Name AlexPosn))

alexInitUserState :: AlexUserState
alexInitUserState = (0, mempty, mempty)

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_ust :: Alex AlexUserState
get_ust = gets_alex alex_ust

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

set_ust :: AlexUserState -> Alex ()
set_ust st = Alex (Right . (go &&& (const ())))
    where go s = s { alex_ust = st }

alexEOF = EOF <$> get_pos

data Sym = PlusTok
         | MinusTok
         | PercentTok
         | FoldTok
         | Quot
         | TimesTok
         | DefEq
         | Colon
         | LBrace
         | RBrace
         | LParen
         | RParen
         | LSqBracket
         | RSqBracket
         | Semicolon
         | Underscore
         | EqTok
         | LeqTok
         | LtTok
         | NeqTok
         | GeqTok
         | GtTok
         | AndTok
         | OrTok
         | Tilde
         | NotMatchTok
         | Comma
         | Dot
         | TallyTok
         | ConstTok
         | LBracePercent
         | LBraceBar
         | Exclamation
         | Caret
         | Backslash
         | BackslashDot
         | FilterTok

instance Pretty Sym where
    pretty PlusTok       = "+"
    pretty MinusTok      = "-"
    pretty PercentTok    = "%"
    pretty FoldTok       = "|"
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
    pretty LSqBracket    = "["
    pretty RSqBracket    = "]"
    pretty Tilde         = "~"
    pretty NotMatchTok   = "!~"
    pretty Comma         = ","
    pretty Dot           = "."
    pretty TallyTok      = "#"
    pretty Quot          = "\""
    pretty Caret         = "^"
    pretty ConstTok      = "[:"
    pretty LBracePercent = "{%"
    pretty LBraceBar     = "{|"
    pretty Exclamation   = "!"
    pretty Backslash     = "\\"
    pretty BackslashDot  = "\\."
    pretty FilterTok     = "#."

data Keyword = KwLet
             | KwIn
             | KwVal
             | KwEnd
             | KwSet
             | KwFn

-- | Reserved/special variables
data Var = VarX
         | VarY
         | VarFs
         | VarIx
         | VarMin
         | VarMax

instance Pretty Var where
    pretty VarX     = "x"
    pretty VarY     = "y"
    pretty VarFs    = "fs"
    pretty VarIx    = "ix"
    pretty VarMin   = "min"
    pretty VarMax   = "max"
    -- TODO: exp, log, sqrt, floor ...

instance Pretty Keyword where
    pretty KwLet = "let"
    pretty KwIn  = "in"
    pretty KwVal = "val"
    pretty KwEnd = "end"
    pretty KwSet = ":set"
    pretty KwFn  = "fn"

data Builtin = BuiltinIParse
             | BuiltinFParse
             | BuiltinSubstr
             | BuiltinSplit
             | BuiltinSplitc
             | BuiltinSprintf
             | BuiltinFloor
             | BuiltinCeil

instance Pretty Builtin where
    pretty BuiltinIParse  = ":i"
    pretty BuiltinFParse  = ":f"
    pretty BuiltinSubstr  = "substr"
    pretty BuiltinSplit   = "split"
    pretty BuiltinSplitc  = "splitc"
    pretty BuiltinSprintf = "sprintf"
    pretty BuiltinFloor   = "floor"
    pretty BuiltinCeil    = "ceil"

data Token a = EOF { loc :: a }
             | TokSym { loc :: a, _sym :: Sym }
             | TokName { loc :: a, _name :: Name a }
             | TokTyName { loc :: a, _tyName :: TyName a }
             | TokBuiltin { loc :: a, _builtin :: Builtin }
             | TokKeyword { loc :: a, _kw :: Keyword }
             | TokResVar { loc :: a, _var :: Var }
             | TokInt { loc :: a, int :: Integer }
             | TokFloat { loc :: a, float :: Double }
             | TokBool { loc :: a, boolTok :: Bool }
             | TokStr { loc :: a, strTok :: BSL.ByteString }
             | TokStreamLit { loc :: a, ix :: Int }
             | TokFieldLit { loc :: a, ix :: Int }
             | TokRR { loc :: a, rr :: BSL.ByteString }
             | TokAccess { loc :: a, ix :: Int }

instance Pretty (Token a) where
    pretty EOF{}              = "(eof)"
    pretty (TokSym _ s)       = "symbol" <+> squotes (pretty s)
    pretty (TokName _ n)      = "identifier" <+> squotes (pretty n)
    pretty (TokTyName _ tn)   = "identifier" <+> squotes (pretty tn)
    pretty (TokBuiltin _ b)   = "builtin" <+> squotes (pretty b)
    pretty (TokKeyword _ kw)  = "keyword" <+> squotes (pretty kw)
    pretty (TokInt _ i)       = pretty i
    pretty (TokStr _ str)     = squotes (pretty $ mkText str)
    pretty (TokStreamLit _ i) = "$" <> pretty i
    pretty (TokFieldLit _ i)  = "`" <> pretty i
    pretty (TokRR _ rr')      = "/" <> pretty (mkText rr') <> "/"
    pretty (TokResVar _ v)    = "reserved variable" <+> squotes (pretty v)
    pretty (TokBool _ True)   = "#t"
    pretty (TokBool _ False)  = "#f"
    pretty (TokAccess _ i)    = "." <> pretty i
    pretty (TokFloat _ f)     = pretty f

freshName :: T.Text -> Alex (Name AlexPosn)
freshName t = do
    pos <- get_pos
    newIdentAlex pos t 

newIdentAlex :: AlexPosn -> T.Text -> Alex (Name AlexPosn)
newIdentAlex pos t = do
    st <- get_ust
    let (st', n) = newIdent pos t st
    set_ust st' $> (n $> pos)

newIdent :: AlexPosn -> T.Text -> AlexUserState -> (AlexUserState, Name AlexPosn)
newIdent pos t pre@(max', names, uniqs) =
    case M.lookup t names of
        Just i -> (pre, Name t (Unique i) pos)
        Nothing -> let i = max' + 1
            in let newName = Name t (Unique i) pos
                in ((i, M.insert t i names, IM.insert i newName uniqs), newName)

loop :: Alex [Token AlexPosn]
loop = do
    tok' <- alexMonadScan
    case tok' of
        EOF{} -> pure []
        _ -> (tok' :) <$> loop

lexJac :: BSL.ByteString -> Either String [Token AlexPosn]
lexJac = flip runAlex loop

runAlexSt :: BSL.ByteString -> Alex a -> Either String (AlexUserState, a)
runAlexSt inp = withAlexSt inp alexInitUserState

withAlexSt :: BSL.ByteString -> AlexUserState -> Alex a -> Either String (AlexUserState, a)
withAlexSt inp ust (Alex f) = first alex_ust <$> f
    (AlexState { alex_bpos = 0
               , alex_pos = alexStartPos
               , alex_inp = inp
               , alex_chr = '\n'
               , alex_ust = ust
               , alex_scd = 0
               })

}
