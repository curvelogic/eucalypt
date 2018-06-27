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

import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Bound.Scope
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
prepare (CorePAp _ f xs) =
  parens $ foldr ((<+>) . prepare) (text "partial:" <+> prepare f) xs
prepare (CoreVar x) = (text . unquote . show) x
prepare (CorePrim x) = (text . renderLiteral) x
prepare (CoreLet bs body) =
  text "let" <+> (vcat binds $$ hang (text "in") 2 prettyBody)
  where
    names = map fst bs
    inst = splat (CoreVar . unquote . show) (\(Name _ i) -> CoreVar (names !! i))
    prettyBody = (prepare . inst) body
    bindExprs = map (prepare . inst . snd) bs
    binds = zipWith (\n b -> text n <+> char '=' <+> b) names bindExprs
prepare (CoreLookup x y) = prepare x <+> char '.' <> text y
prepare (CoreBlock e) = braces $ prepare e
prepare (CoreList [k@(CorePrim (CoreSymbol _)), v]) = brackets (prepare k <> comma <> prepare v)
prepare (CoreList xs) = brackets . vcat . punctuate comma $ map prepare xs
prepare (CoreMeta m e) = vcat [text "`" <+> prepare m, prepare e]
prepare (CoreTraced v) = prepare v
prepare (CoreChecked _ v) = prepare v
prepare (CoreOpSoup es) = parens ( hsep $ map prepare es)
prepare (CoreArgTuple xs) = parens . hsep . punctuate comma $ map prepare xs
prepare (CoreLambda arity e) =
  parens $ text "\\" <+> hsep (V.toList (V.map text argNames)) <+> text "->" <+> body
  where
    pairs = map pair $ bindings e
    pair (Name n b) = (b, n)
    toBindingName = fromMaybe "?" . (`lookup` pairs)
    argNames = V.generate arity toBindingName
    body = prepare $ inst e
    inst =
      splat
        (CoreVar . unquote . show)
        (\(Name nm _) -> CoreVar $ unquote $ show nm)
prepare (CoreApply f es) = prepare f <> parens ( hsep . punctuate comma $ map prepare es)
prepare (CoreName n) = text n
prepare (CoreOperator _x _p e) = prepare e

-- | Pretty Print a CoreExp to String
pprint :: Show a => CoreExp a -> String
pprint = render . prepare
