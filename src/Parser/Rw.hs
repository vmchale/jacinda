module Parser.Rw ( rwP
                 , rwD
                 , rwE
                 ) where


import           A
import           Control.Recursion (cata, embed)

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
mFi NotMatches = Just 5
mFi Min        = Nothing
mFi Max        = Nothing
mFi Split      = Nothing
mFi Splitc     = Nothing
mFi Sprintf    = Nothing
mFi Match      = Nothing
mFi Prior      = Just 5
mFi DedupOn    = Just 5

isPre :: BUn -> Bool
isPre At{}     = False
isPre Select{} = False
isPre IParse   = False
isPre FParse   = False
isPre Parse    = False
isPre _        = True

rwE :: E a -> E a
rwE = cata a where
    a (EAppF l e0@(UB _ op) (EApp lϵ (EApp lϵϵ e1@(BB _ bop) e2) e3))
        | Just{} <- mFi bop
        , isPre op && op /= Dedup
                                                                                        = EApp l (EApp lϵ e1 (EApp lϵϵ e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ op0) _) (EApp l1 (EApp l2 e1@(BB _ op1) e2) e3))
        | Just f0 <- mFi op0
        , Just f1 <- mFi op1
        , f0 > f1
                                                                                        = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@Var{} (EApp lϵ (EApp lϵϵ e1 e2) e3))                                  = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    -- TODO rewrite dfn
    a (EAppF l e0@Var{} (EApp l0 e1 (EApp l1 (EApp l2 op@BB{} e2) e3)))                 = EApp l1 (EApp l2 op (EApp l (EApp l0 e0 e1) e2)) e3
    a (EAppF l e0@Var{} (EApp lϵ e1 e2))                                                = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(BB _ op) (EApp lϵ e1 e2)) | Nothing <- mFi op                        = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(TB _ Substr) (EApp lϵ (EApp lϵϵ e1 e2) e3))                          = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Substr) (EApp lϵ e1 (EApp lϵϵ e2 e3)))                          = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Option) (EApp lϵ (EApp lϵϵ e1 e2) e3))                          = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Option) (EApp lϵ e1 (EApp lϵϵ e2 e3)))                          = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Option) (EApp lϵ e1 e2))                                        = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(TB _ Captures) (EApp lϵ (EApp lϵϵ e1 e2) e3))                        = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Captures) (EApp lϵ e1 (EApp lϵϵ e2 e3)))                        = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ AllCaptures) (EApp lϵ (EApp lϵϵ e1 e2) e3))                     = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ AllCaptures) (EApp lϵ e1 (EApp lϵϵ e2 e3)))                     = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l (RwB l0 b) (EApp lϵ e1 e2))                                              = EApp l (EApp lϵ (BB l0 b) e1) e2
    a (EAppF l (RwT l0 t) (EApp lϵ (EApp lϵϵ e1 e2) e3))                                = EApp l (EApp lϵ (EApp lϵϵ (TB l0 t) e1) e2) e3
    a (EAppF l (RwT l0 t) (EApp lϵ e1 (EApp lϵϵ e2 e3)))                                = EApp l (EApp lϵ (EApp lϵϵ (TB l0 t) e1) e2) e3
    a x                                                                                 = embed x
