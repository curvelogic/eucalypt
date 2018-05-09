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

import Bound.Scope (foldMapBound)
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
prepare (CoreLam scope) = parens $ text "\\" <+> text n <> char '.' <+> body
  where
    n = foldMapBound name scope
    body = prepare (instantiate1Name (CoreVar n) scope)
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

-- | Pretty Print a CoreExp to String
pprint :: CoreExpr -> String
pprint = render . prepare
