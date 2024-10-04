{
    {-# LANGUAGE OverloadedStrings #-}
    module Parser ( pLit
                  , parse
                  , parseWithMax
                  , parseWithCtx
                  , parseLibWithCtx
                  , ParseError (..)
                  -- * Type synonyms
                  , File
                  , Library
                  ) where

import Control.Exception (Exception, throw)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy as BSL
import Data.Functor (void)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import A
import L
import Nm hiding (loc)
import qualified Nm
import Prettyprinter (Pretty (pretty), (<+>), concatWith, squotes)

}

%name parseF File
%name parseLib Library
%name parseL L
%tokentype { Token AlexPosn }
%errorhandlertype explist
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
    loctothorpe { TokSym $$ LBraceOctothorpe }
    lanchor { TokSym $$ LAnchor }
    semicolon { TokSym $$ Semicolon }
    backslash { TokSym $$ Backslash }
    questionMark { TokSym $$ QuestionMark }
    tilde { TokSym $$ Tilde }
    notMatch { TokSym $$ NotMatchTok }
    dot { TokSym $$ Dot }
    lbracePercent { TokSym $$ LBracePercent }
    lbraceBar { TokSym $$ LBraceBar }
    tally { TokSym $$ TallyTok }
    tallyL { TokSym $$ LengthTok }
    const { TokSym $$ ConstTok }
    filter { TokSym $$ FilterTok }
    exclamation { TokSym $$ Exclamation }
    backslashdot { TokSym $$ BackslashDot }
    at { $$@(TokAccess _ _) }
    select { $$@(TokSelect _ _) }
    get { $$@(TokR _ _) }
    floorSym { TokSym $$ FloorSym }
    ceilSym { TokSym $$ CeilSym }
    dedup { TokSym $$ DedupTok }
    dedupon { TokSym $$ DedupOnTok }
    report { TokSym $$ IceCreamCone }

    plus { TokSym $$ PlusTok }
    minus { TokSym $$ MinusTok }
    times { TokSym $$ TimesTok }
    percent { TokSym $$ PercentTok }
    exp { TokSym $$ ExpTok }

    comma { TokSym $$ Comma }
    doubleComma { TokSym $$ DoubleComma }
    dotEq { TokSym $$ DotEq }
    fold { TokSym $$ FoldTok }
    fold1 { TokSym $$ Fold1Tok }
    caret { TokSym $$ Caret }
    amp { TokSym $$ AmpAmp }
    z { TokSym $$ Zilde }
    quot { TokSym $$ Quot }
    mapMaybe { TokSym $$ MapMaybeTok }
    catMaybes { TokSym $$ CatMaybesTok }
    capture { TokSym $$ CapTok }
    mmatch { TokSym $$ L.MMatch }
    neg { TokSym $$ NegTok }

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
    lastField { TokSym $$ LastFieldTok }
    listField { TokSym $$ FieldListTok }

    let { TokKeyword $$ KwLet }
    in { TokKeyword $$ KwIn }
    val { TokKeyword $$ KwVal }
    end { TokKeyword $$ KwEnd }
    set { TokKeyword $$ KwSet }
    flush { TokKeyword $$ KwFlush }
    fn { TokKeyword $$ KwFn }
    include { TokKeyword $$ KwInclude }
    if { TokKeyword $$ KwIf }
    then { TokKeyword $$ KwThen }
    else { TokKeyword $$ KwElse }
    usv { TokKeyword $$ KwUsv }
    asv { TokKeyword $$ KwAsv }
    csv { TokKeyword $$ KwCsv }

    x { TokResVar $$ VarX }
    y { TokResVar $$ VarY }

    min { TokResVar $$ VarMin }
    max { TokResVar $$ VarMax }
    ix { TokResVar $$ VarIx }
    nf { TokResVar $$ VarNf }
    fs { TokResVar $$ VarFs }
    rs { TokResVar $$ VarRs }
    ofs { TokResVar $$ VarOfs }
    ors { TokResVar $$ VarOrs }

    split { TokBuiltin $$ BuiltinSplit }
    splitc { TokBuiltin $$ BuiltinSplitc }
    substr { TokBuiltin $$ BuiltinSubstr }
    sub1 { TokBuiltin $$ BuiltinSub1 }
    subs { TokBuiltin $$ BuiltinSubs }
    sprintf { TokBuiltin $$ BuiltinSprintf }
    floor { TokBuiltin $$ BuiltinFloor }
    ceil { TokBuiltin $$ BuiltinCeil }
    option { TokBuiltin $$ BuiltinOption }
    match { TokBuiltin $$ BuiltinMatch }
    some { TokBuiltin $$ BuiltinSome }
    none { TokBuiltin $$ BuiltinNone }
    fp { TokBuiltin $$ BuiltinFp }
    captures { TokBuiltin $$ BuiltinCaptures }
    mapMaybeL { TokBuiltin $$ BuiltinMapMaybe }
    dedupOnL { TokBuiltin $$ BuiltinDedupOn }
    filterL { TokBuiltin $$ BuiltinFilt }
    foldL { TokBuiltin $$ BuiltinFold }
    fold1L { TokBuiltin $$ BuiltinFold1 }
    scanL { TokBuiltin $$ BuiltinScan }

    head { TokBuiltin $$ BuiltinHead }
    tail { TokBuiltin $$ BuiltinTail }
    init { TokBuiltin $$ BuiltinInit }
    last { TokBuiltin $$ BuiltinLast }
    take { TokBuiltin $$ BuiltinTake }
    drop { TokBuiltin $$ BuiltinDrop }
    rein { TokBuiltin $$ BuiltinRein }

    iParse { TokBuiltin $$ BuiltinIParse }
    fParse { TokBuiltin $$ BuiltinFParse }

    rr { $$@(TokRR _ _) }

%nonassoc leq geq gt lt neq eq
%left plus minus
%left times percent

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
     | gt { Gt } | lt { Lt }
     | geq { Geq } | leq { Leq }
     | eq { Eq } | neq { Neq }
     | quot { Map }
     | mapMaybe { MapMaybe }
     | tilde { Matches } | notMatch { NotMatches }
     | mmatch { A.MMatch }
     | and { And }
     | or { Or }
     | backslashdot { Prior }
     | filter { Filter }
     | fold1 { Fold1 }
     | exp { Exp }
     | dedupon { DedupOn }
     | report { Report }
     | amp { Nier }

Bind :: { (Nm AlexPosn, E AlexPosn) }
     : val name defEq E { ($2, $4) }

Rec :: { (Nm AlexPosn, E AlexPosn) }
    : name dotEq E { ($1, $3) }

Args :: { [(Nm AlexPosn)] }
     : lparen rparen { [] }
     | parens(name) { [$1] }
     | parens(sepBy(name, comma)) { reverse $1 }

D :: { D AlexPosn }
  : set fs defEq rr semicolon { SetFS (rr $4) }
  | set rs defEq rr semicolon { SetRS (rr $4) }
  | set ofs defEq strLit semicolon { SetOFS (strTok $4) }
  | set ors defEq strLit semicolon { SetORS (strTok $4) }
  | set asv semicolon { SetAsv }
  | set usv semicolon { SetUsv }
  | set csv semicolon { SetCsv }
  | flush semicolon { FlushDecl }
  | fn name Args defEq E semicolon { FunDecl $2 $3 $5 }
  | fn name defEq E semicolon { FunDecl $2 [] $4 }

Include :: { FilePath }
        : include strLit { T.unpack (strTok $2) }

File :: { ([FilePath], Program AlexPosn) }
     : many(Include) Program { (reverse $1, $2) }

Library :: { Library }
        : many(Include) many(D) { (reverse $1, reverse $2) }

Program :: { Program AlexPosn }
        : many(D) E { Program (reverse $1) $2 }

L :: { (AlexPosn, L) }
  : intLit { (loc $1, ILit (int $1)) }
  | floatLit { (loc $1, FLit (float $1)) }
  | boolLit { (loc $1, BLit (boolTok $1)) }
  | strLit { (loc $1, StrLit (encodeUtf8 $ strTok $1)) }

E :: { E AlexPosn }
  : name { Var (Nm.loc $1) $1 }
  | column { Column (loc $1) (ix $1) }
  | field { Field (loc $1) (ix $1) }
  | allColumn { AllColumn $1 }
  | allField { AllField $1 }
  | lastField { LastField $1 }
  | listField { FieldList $1 }
  | L { Lit (fst $1) (snd $1) }
  | field iParse { EApp (loc $1) (UB $2 IParse) (Field (loc $1) (ix $1)) }
  | field fParse { EApp (loc $1) (UB $2 FParse) (Field (loc $1) (ix $1)) }
  | name iParse { EApp (Nm.loc $1) (UB $2 IParse) (Var (Nm.loc $1) $1) }
  | name fParse { EApp (Nm.loc $1) (UB $2 FParse) (Var (Nm.loc $1) $1) }
  | field colon { EApp (loc $1) (UB $2 Parse) (Field (loc $1) (ix $1)) }
  | name colon { EApp (Nm.loc $1) (UB $2 Parse) (Var (Nm.loc $1) $1) }
  | lastField iParse { EApp $1 (UB $2 IParse) (LastField $1) }
  | lastField fParse { EApp $1 (UB $2 FParse) (LastField $1) }
  | lastField colon { EApp $1 (UB $2 Parse) (LastField $1) }
  | allField fParse { EApp $1 (UB $2 FParse) (AllField $1) }
  | allField iParse { EApp $1 (UB $2 IParse) (AllField $1) }
  | allField colon { EApp $1 (UB $2 Parse) (AllField $1) }
  | x colon { EApp $1 (UB $2 Parse) (ResVar $1 X) }
  | y colon { EApp $1 (UB $2 Parse) (ResVar $1 Y) }
  | x iParse { EApp $1 (UB $2 IParse) (ResVar $1 X) }
  | x fParse { EApp $1 (UB $2 FParse) (ResVar $1 X) }
  | y iParse { EApp $1 (UB $2 IParse) (ResVar $1 Y) }
  | y fParse { EApp $1 (UB $2 FParse) (ResVar $1 Y) }
  | column iParse { IParseCol (loc $1) (ix $1) }
  | column fParse { FParseCol (loc $1) (ix $1) }
  | column colon { ParseCol (loc $1) (ix $1) }
  | allColumn iParse { IParseAllCol $1 }
  | allColumn fParse { FParseAllCol $1 }
  | allColumn colon { ParseAllCol $1 }
  | parens(iParse) { UB $1 IParse }
  | parens(fParse) { UB $1 FParse }
  | parens(colon) { UB $1 Parse }
  | lparen BBin rparen { BB $1 $2 }
  | lparen E BBin rparen { EApp $1 (BB $1 $3) $2 }
  | lparen BBin E rparen {% do { n <- lift $ freshName "x" ; pure (Lam $1 n (EApp $1 (EApp $1 (BB $1 $2) (Var (Nm.loc n) n)) $3)) } }
  | E BBin E { EApp (eLoc $1) (EApp (eLoc $3) (BB (eLoc $1) $2) $1) $3 }
  | E fold E E { EApp (eLoc $1) (EApp (eLoc $1) (EApp $2 (TB $2 Fold) $1) $3) $4 }
  | E capture E E { EApp (eLoc $1) (EApp (eLoc $1) (EApp $2 (TB $2 Captures) $1) $3) $4 }
  | E caret E E { EApp (eLoc $1) (EApp (eLoc $1) (EApp $2 (TB $2 Scan) $1) $3) $4 }
  | comma E E E { EApp $1 (EApp $1 (EApp $1 (TB $1 ZipW) $2) $3) $4 }
  | lbrace E rbrace braces(E) { Guarded $1 $2 $4 }
  | lbracePercent E rbrace braces(E) { let tl = eLoc $2 in Guarded $1 (EApp tl (EApp tl (BB tl Matches) (AllField tl)) $2) $4 }
  | lbraceBar E rbrace { Implicit $1 $2 }
  | let many(Bind) in E end { mkLet $1 (reverse $2) $4 }
  | lparen sepBy(E,dot) rparen { Tup $1 (reverse $2) }
  | lanchor sepBy(E, dot) rparen { Anchor $1 (reverse $2) }
  | loctothorpe sepBy(Rec,semicolon) rbrace { Rec $1 (reverse $2) }
  | E E { EApp (eLoc $1) $1 $2 }
  | E doubleComma E E { EApp (eLoc $1) (EApp (eLoc $1) (EApp $2 (TB $2 Bookend) $1) $3) $4 }
  | tally { UB $1 Tally }
  | tallyL { UB $1 TallyList }
  | const { UB $1 Const }
  | exclamation { UB $1 Not }
  | lsqbracket E rsqbracket { Dfn $1 $2 }
  | x { ResVar $1 X }
  | y { ResVar $1 Y }
  | rr { RegexLit (loc $1) (encodeUtf8 $ rr $1) }
  | min { BB $1 Min } | max { BB $1 Max }
  | drop { BB $1 Drop } | take { BB $1 Take }
  | rein { BB $1 Rein }
  | mapMaybeL { RwB $1 MapMaybe }
  | dedupOnL { RwB $1 DedupOn }
  | filterL { RwB $1 Filter }
  | foldL { RwT $1 Fold }
  | fold1L { RwB $1 Fold1 }
  | scanL { RwT $1 Scan }
  | split { BB $1 Split }
  | match { BB $1 Match }
  | splitc { BB $1 Splitc }
  | substr { TB $1 Substr }
  | sub1 { TB $1 Sub1 }
  | subs { TB $1 Subs }
  | sprintf { BB $1 Sprintf }
  | option { TB $1 Option }
  | captures { TB $1 AllCaptures }
  | floor { UB $1 Floor }
  | ceil { UB $1 Ceiling }
  | floorSym { UB $1 Floor } | ceilSym { UB $1 Ceiling }
  | dedup { UB $1 Dedup }
  | some { UB $1 Some }
  | catMaybes { UB $1 CatMaybes }
  | head {UB $1 Head} | tail {UB $1 Tail}
  | last {UB $1 Last} | init {UB $1 Init}
  | neg { UB $1 Negate }
  | ix { NB $1 Ix }
  | z { NB $1 MZ }
  | nf { NB $1 Nf }
  | none { NB $1 None }
  | fp { NB $1 Fp }
  | parens(at) { UB (loc $1) (At $ ix $1) }
  | parens(select) { UB (loc $1) (Select $ field $1) }
  | parens(get) { UB (loc $1) (SelR (void $ nfield $1)) }
  | E at { EApp (eLoc $1) (UB (loc $2) (At $ ix $2)) $1 }
  | E select { EApp (eLoc $1) (UB (loc $2) (Select $ field $2)) $1 }
  | E get { EApp (eLoc $1) (UB (loc $2) (SelR (void $ nfield $2))) $1 }
  | backslash name dot E { Lam $1 $2 $4 }
  | parens(E) { Paren (eLoc $1) $1 }
  | if E then E else E { Cond $1 $2 $4 $6 }
  | questionMark E semicolon E semicolon E { Cond $1 $2 $4 $6 }

{

type File = ([FilePath], Program AlexPosn)

type Library = ([FilePath], [D AlexPosn])

parseError :: (Token AlexPosn, [String]) -> Parse a
parseError = throwError . uncurry Unexpected

mkLet :: a -> [(Nm a, E a)] -> E a -> E a
mkLet _ [] e     = e
mkLet l (b:bs) e = Let l b (mkLet l bs e)

data ParseError a = Unexpected !(Token a) [String]
                  | LexErr String

instance Pretty a => Pretty (ParseError a) where
    pretty (Unexpected tok valid) = pretty (loc tok) <+> "Unexpected" <+> pretty tok <> "." <+> "Expected one of" <+> concatWith (\x y -> x <> "," <+> y) (squotes.pretty<$>valid)
    pretty (LexErr str)           = pretty (T.pack str)

instance Pretty a => Show (ParseError a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (ParseError a)

type Parse = ExceptT (ParseError AlexPosn) Alex

parseThrow :: Parse a -> T.Text -> a
parseThrow p = snd.either throw id.runParse p

pLit :: T.Text -> L
pLit = snd.parseThrow parseL

df :: T.Text -> L -> Alex (E AlexPosn -> E AlexPosn)
df t x = do {nm <- newVarAlex t; let l=Nm.loc nm in pure (Let l (nm, Lit l x))}

parse :: T.Text -> Either (ParseError AlexPosn) File
parse = fmap snd . runParse parseF

parseWithMax :: T.Text -> Either (ParseError AlexPosn) (Int, File)
parseWithMax = fmap (first fst3) . runParse parseF
    where fst3 (x, _, _) = x

binds :: [(T.Text, L)] -> Alex (E AlexPosn -> E AlexPosn)
binds = fmap thread.traverse (uncurry df) where thread = foldr (.) id

parseCli :: [(T.Text, L)] -> Parse File
parseCli vs = lift (fmap (second.mapExpr) (binds vs)) <*> parseF

parseWithCtx :: T.Text -> [(T.Text, L)] -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, File)
parseWithCtx src vars = parseWithInitSt (parseCli vars) src

parseLibWithCtx :: T.Text -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, Library)
parseLibWithCtx = parseWithInitSt parseLib

runParse :: Parse a -> T.Text -> Either (ParseError AlexPosn) (AlexUserState, a)
runParse parser str = liftErr $ runAlexSt str (runExceptT parser)

parseWithInitSt :: Parse a -> T.Text -> AlexUserState -> Either (ParseError AlexPosn) (AlexUserState, a)
parseWithInitSt parser str st = liftErr $ withAlexSt str st (runExceptT parser)

liftErr :: Either String (b, Either (ParseError a) c) -> Either (ParseError a) (b, c)
liftErr (Left err)            = Left (LexErr err)
liftErr (Right (_, Left err)) = Left err
liftErr (Right (i, Right x))  = Right (i, x)

}
