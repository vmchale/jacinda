{
    {-# LANGUAGE OverloadedStrings #-}
    module Jacinda.Parser ( parse
                          , parseWithMax
                          , parseWithCtx
                          , parseWithInitCtx
                          , ParseError (..)
                          ) where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified Intern.Name as Name
import Intern.Name hiding (loc)
import Jacinda.AST
import Jacinda.Lexer
import Prettyprinter (Pretty (pretty), (<+>))

}

%name parseP Program
%tokentype { Token AlexPosn }
%error { parseError }
%monad { Parse } { (>>=) } { pure }
%lexer { lift alexMonadScan >>= } { EOF _ }

%token

    defEq { TokSym $$ DefEq }
    colon { TokSym $$ Colon }
    lbrace { TokSym $$ LBrace }
    rbrace { TokSym $$ RBrace }
    lsqbracket { TokSym $$ LSqBracket }
    rsqbracket { TokSym $$ RSqBracket }
    lparen { TokSym $$ LParen }
    rparen { TokSym $$ RParen }
    semicolon { TokSym $$ Semicolon }
    backslash { TokSym $$ Backslash }
    tilde { TokSym $$ Tilde }
    notMatch { TokSym $$ NotMatchTok }
    dot { TokSym $$ Dot }
    lbracePercent { TokSym $$ LBracePercent }
    lbraceBar { TokSym $$ LBraceBar }
    tally { TokSym $$ TallyTok }
    const { TokSym $$ ConstTok }
    filter { TokSym $$ FilterTok }
    exclamation { TokSym $$ Exclamation }
    backslashdot { TokSym $$ BackslashDot }
    at { $$@(TokAccess _ _) }
    select { $$@(TokSelect _ _) }
    floorSym { TokSym $$ FloorSym }
    ceilSym { TokSym $$ CeilSym }

    plus { TokSym $$ PlusTok }
    minus { TokSym $$ MinusTok }
    times { TokSym $$ TimesTok }
    percent { TokSym $$ PercentTok }

    comma { TokSym $$ Comma }
    fold { TokSym $$ FoldTok }
    caret { TokSym $$ Caret }
    quot { TokSym $$ Quot }

    eq { TokSym $$ EqTok }
    neq { TokSym $$ NeqTok }
    leq { TokSym $$ LeqTok }
    lt { TokSym $$ LtTok }
    geq { TokSym $$ GeqTok }
    gt { TokSym $$ GtTok }

    and { TokSym $$ AndTok }
    or { TokSym $$ OrTok }

    name { TokName _ $$ }
    tyName { TokTyName  _ $$ }

    intLit { $$@(TokInt _ _) }
    floatLit { $$@(TokFloat _ _) }
    boolLit { $$@(TokBool _ _) }
    strLit { $$@(TokStr _ _) }
    allColumn { TokStreamLit $$ 0 }
    allField { TokFieldLit $$ 0 }
    column { $$@(TokStreamLit _ _) }
    field { $$@(TokFieldLit _ _) }

    let { TokKeyword $$ KwLet }
    in { TokKeyword $$ KwIn }
    val { TokKeyword $$ KwVal }
    end { TokKeyword $$ KwEnd }
    set { TokKeyword $$ KwSet }
    fn { TokKeyword $$ KwFn }

    x { TokResVar $$ VarX }
    y { TokResVar $$ VarY }

    min { TokResVar $$ VarMin }
    max { TokResVar $$ VarMax }
    ix { TokResVar $$ VarIx }
    fs { TokResVar $$ VarFs }

    split { TokBuiltin $$ BuiltinSplit }
    splitc { TokBuiltin $$ BuiltinSplitc }
    substr { TokBuiltin $$ BuiltinSubstr }
    sprintf { TokBuiltin $$ BuiltinSprintf }
    floor { TokBuiltin $$ BuiltinFloor }
    ceil { TokBuiltin $$ BuiltinCeil }
    option { TokBuiltin $$ BuiltinOption }
    match { TokBuiltin $$ BuiltinMatch }

    iParse { TokBuiltin $$ BuiltinIParse }
    fParse { TokBuiltin $$ BuiltinFParse }

    rr { $$@(TokRR _ _) }

%right const
%left paren iParse fParse
%nonassoc leq geq gt lt neq eq

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

sepBy(p,q)
    : sepBy(p,q) q p { $3 : $1 }
    | p q p { $3 : [$1] }

braces(p)
    : lbrace p rbrace { $2 }

brackets(p)
    : lsqbracket p rsqbracket { $2 }

parens(p)
    : lparen p rparen { $2 }

-- binary operator
BBin :: { BBin }
     : plus { Plus }
     | times { Times }
     | minus { Minus }
     | percent { Div }
     | gt { Gt }
     | lt { Lt }
     | geq { Geq }
     | leq { Leq }
     | eq { Eq }
     | neq { Neq }
     | quot { Map }
     | tilde { Matches }
     | notMatch { NotMatches }
     | and { And }
     | or { Or }
     | backslashdot { Prior }
     | filter { Filter }

Bind :: { (Name AlexPosn, E AlexPosn) }
     : val name defEq E { ($2, $4) }

Args :: { [(Name AlexPosn)] }
     : lparen rparen { [] }
     | parens(name) { [$1] }
     | parens(sepBy(name, comma)) { reverse $1 }

D :: { D AlexPosn }
  : set fs defEq rr semicolon { SetFS (BSL.toStrict $ rr $4) }
  | fn name Args defEq E semicolon { FunDecl $2 $3 $5 }

Program :: { Program AlexPosn }
        : many(D) E { Program (reverse $1) $2 }

E :: { E AlexPosn }
  : name { Var (Name.loc $1) $1 }
  | intLit { IntLit (loc $1) (int $1) }
  | floatLit { FloatLit (loc $1) (float $1) }
  | boolLit { BoolLit (loc $1) (boolTok $1) }
  | strLit { StrLit (loc $1) (strTok $1) }
  | column { Column (loc $1) (ix $1) }
  | field { Field (loc $1) (ix $1) }
  | allColumn { AllColumn $1 }
  | allField { AllField $1 }
  | field iParse { EApp (loc $1) (UBuiltin $2 IParse) (Field (loc $1) (ix $1)) }
  | field fParse { EApp (loc $1) (UBuiltin $2 FParse) (Field (loc $1) (ix $1)) }
  | name iParse { EApp (Name.loc $1) (UBuiltin $2 IParse) (Var (Name.loc $1) $1) }
  | name fParse { EApp (Name.loc $1) (UBuiltin $2 FParse) (Var (Name.loc $1) $1) }
  | field colon { EApp (loc $1) (UBuiltin $2 Parse) (Field (loc $1) (ix $1)) }
  | name colon { EApp (Name.loc $1) (UBuiltin $2 Parse) (Var (Name.loc $1) $1) }
  | x colon { EApp $1 (UBuiltin $2 Parse) (ResVar $1 X) }
  | y colon { EApp $1 (UBuiltin $2 Parse) (ResVar $1 Y) }
  | x iParse { EApp $1 (UBuiltin $2 IParse) (ResVar $1 X) }
  | x fParse { EApp $1 (UBuiltin $2 FParse) (ResVar $1 X) }
  | y iParse { EApp $1 (UBuiltin $2 IParse) (ResVar $1 Y) }
  | y fParse { EApp $1 (UBuiltin $2 FParse) (ResVar $1 Y) }
  | column iParse { IParseCol (loc $1) (ix $1) }
  | column fParse { FParseCol (loc $1) (ix $1) }
  | lparen BBin rparen { BBuiltin $1 $2 }
  | lparen E BBin rparen { EApp $1 (BBuiltin $1 $3) $2 }
  | lparen BBin E rparen {% do { n <- lift $ freshName "x" ; pure (Lam $1 n (EApp $1 (EApp $1 (BBuiltin $1 $2) (Var (Name.loc n) n)) $3)) } }
  | E BBin E { EApp (eLoc $1) (EApp (eLoc $3) (BBuiltin (eLoc $1) $2) $1) $3 }
  | E fold E E { EApp (eLoc $1) (EApp (eLoc $1) (EApp $2 (TBuiltin $2 Fold) $1) $3) $4 }
  | E caret E E { EApp (eLoc $1) (EApp (eLoc $1) (EApp $2 (TBuiltin $2 Scan) $1) $3) $4 }
  | comma E E E { EApp $1 (EApp $1 (EApp $1 (TBuiltin $1 ZipW) $2) $3) $4 }
  | lbrace E rbrace braces(E) { Guarded $1 $2 $4 }
  | lbracePercent E rbrace braces(E) { let tl = eLoc $2 in Guarded $1 (EApp tl (EApp tl (BBuiltin tl Matches) (AllField tl)) $2) $4 }
  | lbraceBar E rbrace { Implicit $1 $2 }
  | let many(Bind) in E end { mkLet $1 (reverse $2) $4 }
  | lparen sepBy(E, dot) rparen { Tup $1 (reverse $2) }
  | E E { EApp (eLoc $1) $1 $2 }
  | tally { UBuiltin $1 Tally }
  | const { UBuiltin $1 Const }
  | exclamation { UBuiltin $1 Not }
  | lsqbracket E rsqbracket { Dfn $1 $2 }
  | x { ResVar $1 X }
  | y { ResVar $1 Y }
  | rr { RegexLit (loc $1) (BSL.toStrict $ rr $1) }
  | min { BBuiltin $1 Min }
  | max { BBuiltin $1 Max }
  | split { BBuiltin $1 Split }
  | match { BBuiltin $1 Match }
  | splitc { BBuiltin $1 Splitc }
  | substr { TBuiltin $1 Substr }
  | sprintf { BBuiltin $1 Sprintf }
  | option { TBuiltin $1 Option }
  | floor { UBuiltin $1 Floor }
  | ceil { UBuiltin $1 Ceiling }
  | floorSym { UBuiltin $1 Floor }
  | ceilSym { UBuiltin $1 Ceiling }
  | ix { NBuiltin $1 Ix }
  | parens(at) { UBuiltin (loc $1) (At $ ix $1) }
  | parens(select) { UBuiltin (loc $1) (Select $ field $1) }
  | E at { EApp (eLoc $1) (UBuiltin (loc $2) (At $ ix $2)) $1 }
  | E select { EApp (eLoc $1) (UBuiltin (loc $2) (Select $ field $2)) $1 }
  | backslash name dot E { Lam $1 $2 $4 }
  | parens(E) { Paren (eLoc $1) $1 }

{

parseError :: Token AlexPosn -> Parse a
parseError = throwError . Unexpected

mkLet :: a -> [(Name a, E a)] -> E a -> E a
mkLet _ [] e     = e
mkLet l (b:bs) e = Let l b (mkLet l bs e)

data ParseError a = Unexpected (Token a)
                  | LexErr String
                  | NoImpl (Name a)

instance Pretty a => Pretty (ParseError a) where
    pretty (Unexpected tok)  = pretty (loc tok) <+> "Unexpected" <+> pretty tok
    pretty (LexErr str)      = pretty (T.pack str)
    pretty (NoImpl n)        = pretty (Name.loc n) <+> "Signature for" <+> pretty n <+> "is not accompanied by an implementation"

instance Pretty a => Show (ParseError a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (ParseError a)

type Parse = ExceptT (ParseError AlexPosn) Alex

parse :: BSL.ByteString -> Either (ParseError AlexPosn) (Program AlexPosn)
parse = fmap snd . runParse parseP

parseWithMax :: BSL.ByteString -> Either (ParseError AlexPosn) (Int, Program AlexPosn)
parseWithMax = fmap (first fst3) . parseWithInitCtx
    where fst3 (x, _, _) = x

parseWithInitCtx :: BSL.ByteString -> Either (ParseError AlexPosn) (AlexUserState, Program AlexPosn)
parseWithInitCtx bsl = parseWithCtx bsl alexInitUserState

parseWithCtx :: BSL.ByteString -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, Program AlexPosn)
parseWithCtx = parseWithInitSt parseP

runParse :: Parse a -> BSL.ByteString -> Either (ParseError AlexPosn) (AlexUserState, a)
runParse parser str = liftErr $ runAlexSt str (runExceptT parser)

parseWithInitSt :: Parse a -> BSL.ByteString -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, a)
parseWithInitSt parser str st = liftErr $ withAlexSt str st (runExceptT parser)
    where liftErr (Left err)            = Left (LexErr err)
          liftErr (Right (_, Left err)) = Left err
          liftErr (Right (i, Right x))  = Right (i, x)

liftErr :: Either String (b, Either (ParseError a) c) -> Either (ParseError a) (b, c)
liftErr (Left err)            = Left (LexErr err)
liftErr (Right (_, Left err)) = Left err
liftErr (Right (i, Right x))  = Right (i, x)

}
