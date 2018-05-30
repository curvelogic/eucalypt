{-|
Module      : Eucalypt.Core.Pretty
Description : CRUDE! pretty printing for debugging purposes
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Pretty
  ( pprint
  ) where

import Bound.Scope (bindings)
import Bound.Name
import Eucalypt.Core.Syn
import Text.PrettyPrint
  ( Doc
  , ($$)
  , (<+>)
  , (<>)
  , braces
  , brackets
  , char
  , comma
  , hang
  , hsep
  , parens
  , punctuate
  , render
  , text
  , vcat
  )

renderLiteral :: Primitive -> String
renderLiteral (CoreInt i) = show i
renderLiteral (CoreFloat f) = show f
renderLiteral (CoreString s) = s
renderLiteral (CoreSymbol s) = ":" ++ s
renderLiteral (CoreBoolean b) = show b
renderLiteral CoreNull = "null"

-- | Generate the format document for rendering
prepare :: CoreExpr -> Doc
prepare (CoreBuiltin n) = text $ "__" ++ n
prepare (CoreApp f x) = parens $ prepare f <+> prepare x
prepare (CorePAp _ f xs) =
  parens $ foldr ((<+>) . prepare) (text "partial:" <+> prepare f) xs
prepare (CoreVar x) = char '$' <> text x
prepare (CorePrim x) = (text . renderLiteral) x
prepare (CoreLet bs body) =
  text "let" <+> (vcat binds $$ hang (text "in") 2 prettyBody)
  where
    names = map fst bs
    prettyBody = prepare (inst body)
    bindExprs = map (prepare . inst . snd) bs
    inst = instantiateName (\n -> CoreVar (names !! n))
    binds = zipWith (\n b -> text n <+> char '=' <+> b) names bindExprs
prepare (CoreLookup x y) = prepare x <+> char '.' <> text y
prepare (CoreBlock e) = braces $ prepare e
prepare (CoreList xs) = brackets . hsep . punctuate comma $ map prepare xs
prepare (CoreMeta m e) = vcat [text "` " <+> prepare m, prepare e]
prepare (CoreTraced v) = prepare v
prepare (CoreChecked _ v) = prepare v
prepare (CoreOpSoup es) = parens $ hsep $ map prepare es
prepare (CoreArgTuple xs) = parens . hsep . punctuate comma $ map prepare xs
prepare (CoreLambda _ e) = parens $ text "\\" <+> hsep (map text ns) <> char '.' <+> body
  where
    ns = map name $ bindings e
    body = prepare $ inst e
    inst = instantiateName (\n -> CoreVar (ns !! n))
prepare (CoreApply f es) = prepare f <+> parens ( hsep . punctuate comma $ map prepare es)
prepare (CoreName n) = text n
prepare (CoreOperator _x _p e) = prepare e

-- | Pretty Print a CoreExp to String
pprint :: CoreExpr -> String
pprint = render . prepare
