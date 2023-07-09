module Parser.Rw ( rwP
                 , rwD
                 , rwE
                 ) where


import           A
import           Control.Recursion (cata, embed)

rwP :: Program a -> Program a
rwP (Program ds e) = Program (rwD <$> ds) (rwE e)

rwD :: D a -> D a
rwD (FunDecl n bs e) = FunDecl n bs (rwE e); rewriteD d = d

rwE :: E a -> E a
rwE = cata a where
    a (EAppF l e0@(UB _ Tally) (EApp lϵ (EApp lϵϵ e1@BB{} e2) e3))                      = EApp l (EApp lϵ e1 (EApp lϵϵ e0 e2)) e3
    a (EAppF l e0@(UB _ Const) (EApp lϵ (EApp lϵϵ e1@(BB _ Map) e2) e3))                = EApp l (EApp lϵ e1 (EApp lϵϵ e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Eq) _) (EApp l1 (EApp l2 e1@(BB _ And) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Eq) _) (EApp l1 (EApp l2 e1@(BB _ Or) e2) e3))          = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Neq) _) (EApp l1 (EApp l2 e1@(BB _ And) e2) e3))        = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Neq) _) (EApp l1 (EApp l2 e1@(BB _ Or) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Gt) _) (EApp l1 (EApp l2 e1@(BB _ And) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Gt) _) (EApp l1 (EApp l2 e1@(BB _ Or) e2) e3))          = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Lt) _) (EApp l1 (EApp l2 e1@(BB _ And) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Lt) _) (EApp l1 (EApp l2 e1@(BB _ Or) e2) e3))          = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Leq) _) (EApp l1 (EApp l2 e1@(BB _ And) e2) e3))        = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Leq) _) (EApp l1 (EApp l2 e1@(BB _ Or) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Geq) _) (EApp l1 (EApp l2 e1@(BB _ And) e2) e3))        = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Geq) _) (EApp l1 (EApp l2 e1@(BB _ Or) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Matches) _) (EApp l1 (EApp l2 e1@(BB _ And) e2) e3))    = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Matches) _) (EApp l1 (EApp l2 e1@(BB _ Or) e2) e3))     = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ NotMatches) _) (EApp l1 (EApp l2 e1@(BB _ And) e2) e3)) = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ NotMatches) _) (EApp l1 (EApp l2 e1@(BB _ Or) e2) e3))  = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Fold1) _) (EApp l1 (EApp l2 e1@(BB _ Eq) e2) e3))       = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Fold1) _) (EApp l1 (EApp l2 e1@(BB _ Neq) e2) e3))      = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Fold1) _) (EApp l1 (EApp l2 e1@(BB _ Gt) e2) e3))       = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Fold1) _) (EApp l1 (EApp l2 e1@(BB _ Geq) e2) e3))      = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Fold1) _) (EApp l1 (EApp l2 e1@(BB _ Leq) e2) e3))      = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BB _ Fold1) _) (EApp l1 (EApp l2 e1@(BB _ Lt) e2) e3))       = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@Var{} (EApp lϵ (EApp lϵϵ e1 e2) e3))                                  = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    -- TODO rewrite dfn
    a (EAppF l e0@Var{} (EApp l0 e1 (EApp l1 (EApp l2 op@BB{} e2) e3)))                 = EApp l1 (EApp l2 op (EApp l (EApp l0 e0 e1) e2)) e3
    a (EAppF l e0@Var{} (EApp lϵ e1 e2))                                                = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(BB _ Max) (EApp lϵ e1 e2))                                           = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(BB _ Min) (EApp lϵ e1 e2))                                           = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(BB _ Split) (EApp lϵ e1 e2))                                         = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(BB _ Match) (EApp lϵ e1 e2))                                         = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(BB _ Splitc) (EApp lϵ e1 e2))                                        = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(BB _ Sprintf) (EApp lϵ e1 e2))                                       = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(TB _ Substr) (EApp lϵ (EApp lϵϵ e1 e2) e3))                          = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Substr) (EApp lϵ e1 (EApp lϵϵ e2 e3)))                          = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Option) (EApp lϵ (EApp lϵϵ e1 e2) e3))                          = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Option) (EApp lϵ e1 (EApp lϵϵ e2 e3)))                          = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ Option) (EApp lϵ e1 e2))                                        = EApp l (EApp lϵ e0 e1) e2
    a (EAppF l e0@(TB _ AllCaptures) (EApp lϵ (EApp lϵϵ e1 e2) e3))                     = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a (EAppF l e0@(TB _ AllCaptures) (EApp lϵ e1 (EApp lϵϵ e2 e3)))                     = EApp l (EApp lϵ (EApp lϵϵ e0 e1) e2) e3
    a x                                                                                 = embed x
