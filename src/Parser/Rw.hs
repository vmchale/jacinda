module Parser.Rw ( rwP, rwD, rwE ) where


import           A
import           Data.Bifunctor (second)

rwP :: Program a -> Program a
rwP (Program ds e) = Program (rwD <$> ds) (rwE e)

rwD :: D a -> D a
rwD (FunDecl n bs e) = FunDecl n bs (rwE e); rwD d = d

mFi :: BBin -> Maybe Int
mFi And        = Just 3
mFi Or         = Just 2
mFi Eq         = Just 4
mFi Geq        = Just 4
mFi Gt         = Just 4
mFi Lt         = Just 4
mFi Leq        = Just 4
mFi Neq        = Just 4
mFi Exp        = Just 8
mFi Plus       = Just 6
mFi Minus      = Just 6
mFi Times      = Just 7
mFi Div        = Just 7
mFi Map        = Just 5
mFi MapMaybe   = Just 5
mFi Filter     = Just 5
mFi Fold1      = Just 5
mFi Matches    = Just 5
mFi MMatch     = Just 5
mFi NotMatches = Just 5
mFi Min        = Nothing
mFi Max        = Nothing
mFi Split      = Nothing
mFi Splitc     = Nothing
mFi Sprintf    = Nothing
mFi Match      = Nothing
mFi Drop       = Nothing
mFi Take       = Nothing
mFi Rein       = Nothing
mFi Prior      = Just 5
mFi DedupOn    = Just 5
mFi Report     = Just 4

pPre :: BUn -> Bool
pPre Dedup     = True
pPre Not       = True
pPre TallyList = True
pPre Tally     = True
pPre _         = False

-- FIXME: prefix-not should extend over vars...

rwE :: E a -> E a
rwE (EApp l0 (EApp l1 (EApp l2 ho@TB{} e3) e2) e1) =
    EApp l0 (EApp l1 (EApp l2 ho (rwE e3)) (rwE e2)) (rwE e1)
rwE (EApp l0 (EApp l1 e0@(BB _ op0) e1) e2) | Just fi <- mFi op0 =
    case rwE e2 of
        (EApp l2 (EApp l3 e3@(BB _ op1) e4) e5) | Just fi' <- mFi op1, fi > fi' -> EApp l0 (EApp l1 e3 (rwE (EApp l2 (EApp l3 e0 e1) e4))) e5
        e2'                                                                     -> EApp l0 (EApp l1 e0 (rwE e1)) e2'
rwE (EApp l op@(UB _ Dedup) e) = EApp l op (rwE e)
rwE (EApp l (RwB l0 b) e') =
    case rwE e' of
        (EApp lϵ e1 e2) -> EApp l (EApp lϵ (BB l0 b) e1) e2
        eϵ              -> EApp l (BB l0 b) eϵ
rwE (EApp l (RwT l0 t) e') =
    case rwE e' of
        (EApp lϵ (EApp lϵϵ e1 e2) e3) -> EApp l (EApp lϵ (EApp lϵϵ (TB l0 t) e1) e2) e3
        (EApp lϵ e1 e2)               -> EApp l (EApp lϵ (TB l0 t) e1) e2
        eϵ                            -> EApp l (TB l0 t) eϵ
rwE (EApp l e0 e') =
    case (e0, rwE e') of
        (_, EApp lϵ (EApp lϵϵ e3@(BB _ op) e4) e2) | Just{} <- mFi op -> EApp l (EApp lϵϵ e3 (rwE $ EApp lϵ e0 e4)) e2
        (UB _ f, e2) | pPre f                                         -> EApp l e0 e2
        (_, EApp lϵ e1@EApp{} e2)                                     -> EApp l (rwE $ EApp lϵ e0 e1) e2
        (_, EApp lϵ e1 e2)                                            -> EApp l (EApp lϵ (rwE e0) e1) e2
        (_, eRw)                                                      -> EApp l (rwE e0) eRw
rwE e@Column{} = e
rwE e@IParseCol{} = e
rwE e@FParseCol{} = e
rwE e@ParseCol{} = e
rwE e@Field{} = e
rwE e@LastField{} = e
rwE e@FieldList{} = e
rwE e@AllField{} = e
rwE e@AllColumn{} = e
rwE e@IParseAllCol{} = e
rwE e@FParseAllCol{} = e
rwE e@ParseAllCol{} = e
rwE F{} = error "impossible."; rwE RC{} = error "impossible."
rwE (Guarded l p e) = Guarded l (rwE p) (rwE e)
rwE (Implicit l e) = Implicit l (rwE e)
rwE (Let l (n, e') e) = Let l (n, rwE e') (rwE e)
rwE e@Var{} = e
rwE e@Lit{} = e
rwE e@RegexLit{} = e
rwE (Lam l n e) = Lam l n (rwE e)
rwE (Dfn l e) = Dfn l (rwE e)
rwE e@BB{} = e
rwE e@TB{} = e
rwE e@UB{} = e
rwE e@NB{} = e
rwE (Tup l es) = Tup l (rwE<$>es)
rwE (Rec l es) = Rec l (second rwE<$>es)
rwE e@ResVar{} = e
rwE (Paren l e) = Paren l (rwE e)
rwE (Cond l p e e') = Cond l (rwE p) (rwE e) (rwE e')
rwE (RwB l b) = BB l b
rwE (RwT l b) = TB l b
