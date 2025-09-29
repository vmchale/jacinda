module Jacinda.Check.Field ( cF, LErr (..) ) where

import           A
import           Control.Applicative (Alternative (..))
import           Control.Exception   (Exception)
import           Data.Foldable       (asum)
import           Prettyprinter       (Pretty (..), squotes, (<+>))

data LErr = NF (E T) | TS (E T) | RS (E T)

instance Pretty LErr where
    pretty (NF e) = "Naked field in expression" <+> squotes (pretty e)
    pretty (TS e) = squotes (pretty e) <+> "Tuples cannot have streams."
    pretty (RS e) = squotes (pretty e) <+> "Records cannot have streams."

instance Show LErr where show=show.pretty

instance Exception LErr where

cF :: E T -> Maybe LErr
cF e@(Tup (TyTup ts) _) | any isS ts = Just (TS e); cF e@(Rec (TyRec rs) _) | any isS rs = Just (RS e)
cF e@Field{} = Just (NF e); cF e@AllField{} = Just (NF e); cF e@LastField{} = Just (NF e); cF e@FieldList{} = Just (NF e)
cF e@(NB _ Ix) = Just (NF e); cF e@(NB _ Nf) = Just (NF e)
cF IParseCol{} = Nothing; cF FParseCol{} = Nothing; cF ParseCol{} = Nothing; cF Column{} = Nothing
cF AllColumn{} = Nothing; cF FParseAllCol{} = Nothing; cF IParseAllCol{} = Nothing; cF ParseAllCol{} = Nothing
cF Guarded{} = Nothing; cF Implicit{} = Nothing
cF Lit{} = Nothing; cF RegexLit{} = Nothing;
cF NB{} = Nothing; cF UB{} = Nothing; cF BB{} = Nothing; cF TB{} = Nothing
cF Var{} = Nothing; cF (Tup _ es) = cF||>es; cF (Anchor _ es) = cF||>es
cF (Arr _ es) = cF||>es; cF (Rec _ es) = (cF.snd) ||> es; cF (EApp _ e e') = cF e <|> cF e'
cF (Cond _ p e e') = cF p <|> cF e <|> cF e'; cF (OptionVal _ e) = cF||>e
cF (Lam _ _ e) = cF e; cF Let{} = error "Inlining unexpectedly failed?"
cF RC{} = error "Sanity check failed. Regex should not be compiled at this time."
cF Dfn{} = desugar; cF Paren{} = desugar; cF ResVar{} = desugar
cF RwB{} = desugar; cF RwT{} = desugar

isS :: T -> Bool
isS (TyB TyStream:$_) = True; isS _ = False

(||>) :: (Traversable t, Alternative f) => (a -> f b) -> t a -> f b
f ||> xs = asum (f <$> xs)

desugar = error "Internal error. Should have been desugared by now."
