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

import Bound
import Control.Monad ( liftM2 )
import Control.Monad.Supply
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
prepare :: CoreExpr -> Supply String Doc

prepare (CoreLam e) = do
  n <- supply
  body <- prepare (instantiate1 (CoreVar n) e)
  return $ parens $ text "\\" <+> text n <> char '.' <+> body

prepare (CoreBuiltin n) = return . text $ "__" ++ n

prepare (CoreApp f x) = parens <$> ((<+>) <$> prepare f <*> prepare x)

prepare (CorePAp _ f xs) =
  parens <$> foldr (liftM2 (<+>) . prepare) (prepare f) xs

prepare (CoreVar x) = return $ char '$' <> text x

prepare (CorePrim x) = return $ (text . renderLiteral) x

prepare (CoreLet bindings body) = do
  -- get the names we need and a fn to instantiate vars from them
  freshNames <- supplies (length bindings)
  let inst = instantiate (\n -> CoreVar (freshNames !! n))

  -- instantiate to fill in names
  prettyBody <- prepare (inst body)
  bindExprs <- mapM (prepare . inst) bindings

  -- format
  let binds = zipWith (\n b -> text n <+> char '=' <+> b) freshNames bindExprs
  return $ text "let" <+> (vcat binds $$ hang (text "in") 2 prettyBody)

prepare (CoreLookup x y) = (<> text y) <$> prepare x

prepare (CoreBlock e) = braces <$> prepare e

prepare (CoreList xs) = brackets . hsep . punctuate comma <$> mapM prepare xs

-- HACK: Proper name supply needed until we use Bound.Name
names :: [CoreBindingName]
names = [ [i] | i <- ['a'..'z']] ++ [i : show j | j <- [1 :: Int ..], i <- ['a'..'z'] ]

-- | Pretty Print a CoreExp to String
pprint :: CoreExpr -> String
pprint = render . (`evalSupply` names) . prepare
