{-|
Module      : Eucalypt.Core.Pretty
Description : Pretty printing of eucalypt core
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Pretty
  (prepare)
where

import Eucalypt.Core.Syn
import Text.PrettyPrint
  ( Doc(..),
    text,
    char,
    brackets,
    parens,
    punctuate,
    comma,
    hang,
    hcat,
    hsep,
    vcat,
    braces,
    empty,
    (<+>),
    (<>),
    ($$)
  )

renderName :: Name -> String
renderName (Local n) = n

renderLiteral :: Primitive -> String
renderLiteral (Int i) = "i:" ++ show i
renderLiteral (Float f) = "f:" ++ show f
renderLiteral (String s) = s
renderLiteral (Symbol s) = ":'" ++ s ++ "'"

-- | Generate the format document for rendering
prepare :: InitialCoreExpr -> Doc
prepare (Lam x expr) = parens $ text "\\" <+> text (renderName x) <> char '.' <+> prepare expr
prepare (App x y) = parens $ prepare x <+> prepare y
prepare (Var x) = char '$' <> (text . renderName) x
prepare (Prim x) = (text . renderLiteral) x
prepare (Let rec bs expr) =
  text keyword <+> (vcat (map binding bs) $$ hang (text "in") 2 (prepare expr))
  where keyword = if rec then "letrec" else "let"
        binding (b, body) = text (renderName b) <+> char '=' <+> prepare body
prepare (Lookup x y) = prepare x <> text (renderName y)
prepare (BlockValue decls) = braces $ vcat (map declare decls)
  where declare (key, expr) = prepare key <+> char ':' <+> prepare expr
prepare (ListValue xs) = brackets $ hsep $ punctuate comma (map prepare xs)
