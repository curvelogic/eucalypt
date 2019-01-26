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
import Data.Maybe (fromMaybe)
import Eucalypt.Core.Syn
import Eucalypt.Core.SourceMap
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
  -- , empty
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
prepare (CoreBuiltin _ n) = text $ "__" ++ n
prepare (CoreVar _ x) = (text . unquote . show) x
prepare (CorePrim _ x) = (text . renderLiteral) x
prepare (CoreLet _ bs body _) =
  text "let" <+> (vcat binds $$ hang (text "in") 2 prettyBody)
  where
    names = map fst bs
    inst = splat (anon CoreVar . unquote . show) (\i -> anon CoreVar (names !! i))
    prettyBody = (prepare . inst) body
    bindExprs = map (prepare . inst . snd) bs
    binds = zipWith (\n b -> text n <+> char '=' <+> b) names bindExprs
prepare (CoreLookup _ x y d) = prepare x <+> char '.' <> text y <> fromMaybe (text "") (prepare <$> d)
prepare (CoreBlock _ e) = braces $ prepare e
prepare (CoreList _ [k@(CorePrim _ (CoreSymbol _)), v]) = brackets (prepare k <> comma <> prepare v)
prepare (CoreList _ xs) = brackets . vcat . punctuate comma $ map prepare xs
prepare (CoreMeta _ m e) = vcat [text "`" <+> prepare m, prepare e]
prepare (CoreOpSoup _ es) = parens ( hsep $ map prepare es)
prepare (CoreArgTuple _ xs) = parens . hsep . punctuate comma $ map prepare xs
prepare (CoreLambda _ _ names e) =
  parens $ text "\\" <+> hsep (V.toList (V.map text argNames)) <+> text "->" <+> body
  where
    argNames = V.fromList names
    body = prepare $ inst e
    inst =
      splat
        (anon CoreVar . unquote . show)
        (anon CoreVar . (names !!))
prepare (CoreApply _ f es) = prepare f <> parens ( hsep . punctuate comma $ map prepare es)
prepare (CoreName _ n) = text n
prepare (CoreOperator _ x p e) =
  char '^' <> text (show x) <> parens (text (show p)) <> char '^' <> prepare e
prepare (CoreUnresolved _ v) = text $ "**********UNRESOLVED: " ++ v ++ "**********"
prepare CoreEliminated = text "**********GONE**********"



-- | Pretty Print a CoreExp to String
pprint :: Show a => CoreExp a -> String
pprint = render . prepare
