module Fmt ( layout, fmt ) where

import           A
import qualified Data.Text                 as T
import           L
import           Parser
import           Prettyprinter             (Doc, defaultLayoutOptions, indent, layoutSmart, pretty, tupled, vsep, (<+>))
import           Prettyprinter.Render.Text (renderStrict)

layout=renderStrict.layoutSmart defaultLayoutOptions

fmt :: T.Text -> Either (ParseError AlexPosn) (Doc ann)
fmt = fmap (fmtP.snd).parseWithMax

fmtP :: ([FilePath], Program Ann) -> Doc ann
fmtP (is, Program ds e) = pI (vsep (fmtD<$>ds) <#> fmtE e)
  where
        pI = case is of [] -> id; _ -> (vsep ((\i -> "@include'"<>pretty i<>"'")<$>is)<##>)

fmtD :: D Ann -> Doc ann
fmtD d = uc $
    case d of
      FunDecl _ n ns e -> "fn" <+> pretty n <> tupled (pretty<$>ns) <+> ":=" <#> indent 2 (pretty e <> ";")
      _                -> pretty d
  where
    uc = case dA d of Ann _ Nothing -> id; Ann _ (Just c) -> (pretty c<#>)

fmtE :: E Ann -> Doc ann
fmtE e = pretty e
