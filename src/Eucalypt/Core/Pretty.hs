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

import qualified Data.Vector as V
import Bound.Scope
import Eucalypt.Core.Syn
import Prelude hiding ((<>))
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
renderLiteral (CoreString s) = "\"" ++ s ++ "\""
renderLiteral (CoreSymbol s) = ":" ++ s
renderLiteral (CoreBoolean b) = show b
renderLiteral CoreNull = "null"

unquote :: String -> String
unquote ('"' : xs) = (reverse . unquote . reverse) xs
unquote xs = xs


-- | Generate the format document for rendering
prepare :: Show a => CoreExp a -> Doc
prepare (CoreBuiltin n) = text $ "__" ++ n
prepare (CoreVar x) = (text . unquote . show) x
prepare (CorePrim x) = (text . renderLiteral) x
prepare (CoreLet bs body) =
  text "let" <+> (vcat binds $$ hang (text "in") 2 prettyBody)
  where
    names = map fst bs
    inst = splat (CoreVar . unquote . show) (\i -> CoreVar (names !! i))
    prettyBody = (prepare . inst) body
    bindExprs = map (prepare . inst . snd) bs
    binds = zipWith (\n b -> text n <+> char '=' <+> b) names bindExprs
prepare (CoreLookup x y) = prepare x <+> char '.' <> text y
prepare (CoreBlock e) = braces $ prepare e
prepare (CoreList [k@(CorePrim (CoreSymbol _)), v]) = brackets (prepare k <> comma <> prepare v)
prepare (CoreList xs) = brackets . vcat . punctuate comma $ map prepare xs
prepare (CoreMeta m e) = vcat [text "`" <+> prepare m, prepare e]
prepare (CoreOpSoup es) = parens ( hsep $ map prepare es)
prepare (CoreArgTuple xs) = parens . hsep . punctuate comma $ map prepare xs
prepare (CoreLambda names e) =
  parens $ text "\\" <+> hsep (V.toList (V.map text argNames)) <+> text "->" <+> body
  where
    argNames = V.fromList names
    body = prepare $ inst e
    inst =
      splat
        (CoreVar . unquote . show)
        (CoreVar . (names !!))
prepare (CoreApply f es) = prepare f <> parens ( hsep . punctuate comma $ map prepare es)
prepare (CoreName n) = text n
prepare (CoreOperator x p e) =
  char '^' <> text (show x) <> parens (text (show p)) <> char '^' <> prepare e

-- | Pretty Print a CoreExp to String
pprint :: Show a => CoreExp a -> String
pprint = render . prepare
