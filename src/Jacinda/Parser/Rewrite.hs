module Jacinda.Parser.Rewrite ( rewriteProgram
                              ) where

import           Control.Recursion (cata, embed)
import           Jacinda.AST

rewriteProgram :: Program a -> Program a
rewriteProgram (Program ds e) = Program (rewriteD <$> ds) (rewriteE e)

rewriteD :: D a -> D a
rewriteD d@SetFS{}        = d
rewriteD (FunDecl n bs e) = FunDecl n bs (rewriteE e)

rewriteE :: E a -> E a
rewriteE = cata a where
    a (EAppF l e0@(UBuiltin _ Tally) (EApp lϵ (EApp lϵϵ e1@BBuiltin{} e2) e3))                      = EApp l (EApp lϵ e1 (EApp lϵϵ e0 e2)) e3
    a (EAppF l e0@(UBuiltin _ Const) (EApp lϵ (EApp lϵϵ e1@(BBuiltin _ Map) e2) e3))                = EApp l (EApp lϵ e1 (EApp lϵϵ e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Eq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Eq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))          = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Neq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))        = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Neq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Gt) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Gt) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))          = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Lt) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Lt) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))          = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Leq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))        = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Leq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Geq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))        = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Geq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Matches) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))    = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Matches) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))     = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ NotMatches) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3)) = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ NotMatches) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))  = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    -- a (EAppF l e0 (EApp lϵ e1 e2))                                                                  = EApp l (EApp lϵ e0 e1) e2
    a x                                                                                             = embed x
