{-|
Module      : Eucalypt.Core.Pretty
Description : CRUDE! pretty printing for debugging purposes
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Pretty
  (pprint)
where

import Eucalypt.Core.Syn
import Bound
import Control.Monad.Supply
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
    render,
    (<+>),
    (<>),
    ($$)
  )

renderLiteral :: Primitive -> String
renderLiteral (Int i) = show i
renderLiteral (Float f) = show f
renderLiteral (String s) = s
renderLiteral (Symbol s) = ":" ++ s

-- | Generate the format document for rendering
prepare :: CoreExpr -> Supply String Doc

prepare (CoreLam e) = do
  n <- supply
  body <- prepare (instantiate1 (CoreVar n) e)
  return $ parens $ text "\\" <+> text n <> char '.' <+> body

prepare (CoreApp f x) = parens <$> ((<+>) <$> prepare f <*> prepare x)

prepare (CoreVar x) = return $ char '$' <> text x

prepare (CorePrim x) = return $ (text . renderLiteral) x

prepare (CoreLet bs b) = do
  -- get the names we need and a fn to instantiate vars from them
  names <- supplies (length bs)
  let inst = instantiate (\n -> CoreVar (names !! n))

  -- instantiate to fill in names
  body <- prepare (inst b)
  bindExprs <- mapM (prepare . inst) bs

  -- format
  let binds = zipWith (\n b -> text n <+> char '=' <+> b) names bindExprs
  return $ text "let" <+> (vcat binds $$ hang (text "in") 2 body)

prepare (CoreLookup x y) = (<> text y) <$> prepare x

prepare (CoreBlock e) = braces <$> prepare e

prepare (CoreList xs) = brackets . hsep . punctuate comma <$> mapM prepare xs

-- HACK: Proper name supply needed until we use Bound.Name
names :: [CoreBindingName]
names = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1 :: Int ..], i <- ['a'..'z'] ]

-- | Pretty Print a CoreExp to String
pprint :: CoreExpr -> String
pprint = render . (`evalSupply` names) . prepare
