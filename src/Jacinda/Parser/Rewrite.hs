module Jacinda.Parser.Rewrite ( rewriteE
                              ) where

import           Control.Recursion (cata, embed)
import           Jacinda.AST

rewriteE :: E a -> E a
rewriteE = cata a where
    a (EAppF l e0@(UBuiltin _ Tally) (EApp lϵ (EApp lϵϵ e1@BBuiltin{} e2) e3))                      = EApp l (EApp lϵ e1 (EApp lϵϵ e0 e2)) e3
    a (EAppF l e0@(UBuiltin _ Const) (EApp lϵ (EApp lϵϵ e1@(BBuiltin _ Map) e2) e3))                = EApp l (EApp lϵ e1 (EApp lϵϵ e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Eq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Eq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))          = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Neq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))        = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Neq) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))         = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Matches) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3))    = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ Matches) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))     = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ NotMatches) _) (EApp l1 (EApp l2 e1@(BBuiltin _ And) e2) e3)) = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    a (EAppF l e0@(EApp _ (BBuiltin _ NotMatches) _) (EApp l1 (EApp l2 e1@(BBuiltin _ Or) e2) e3))  = EApp l1 (EApp l2 e1 (EApp l e0 e2)) e3
    -- a (EAppF l e0 (EApp lϵ e1 e2))                                                          = EApp l (EApp lϵ e0 e1) e2
    a x                                                                                             = embed x
