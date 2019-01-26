{-|
Module      : Eucalypt.Core.AnonSyn
Description : Core expression constructors for exprs with no SourceMap ids
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.AnonSyn where

import Eucalypt.Core.SourceMap
import qualified Eucalypt.Core.Syn as Syn

bif :: String -> Syn.CoreExp a
bif = anon Syn.bif

infixl_ :: Syn.Precedence -> Syn.CoreExp a -> Syn.CoreExp a
infixl_ = anon Syn.infixl_

infixr_ :: Syn.Precedence -> Syn.CoreExp a -> Syn.CoreExp a
infixr_ = anon Syn.infixr_

prefix_ :: Syn.Precedence -> Syn.CoreExpr -> Syn.CoreExpr
prefix_ = anon Syn.prefix_

postfix_ :: Syn.Precedence -> Syn.CoreExpr -> Syn.CoreExpr
postfix_ = anon Syn.postfix_

var :: a -> Syn.CoreExp a
var = anon Syn.var

int :: Integer -> Syn.CoreExp a
int = anon Syn.int

lam :: [Syn.CoreBindingName] -> Syn.CoreExpr -> Syn.CoreExpr
lam = anon Syn.lam

letexp :: [(Syn.CoreBindingName, Syn.CoreExpr)] -> Syn.CoreExpr -> Syn.CoreExpr
letexp = anon Syn.letexp

letblock :: [(Syn.CoreBindingName, Syn.CoreExpr)] -> Syn.CoreExpr -> Syn.CoreExpr
letblock = anon Syn.letblock

app :: Syn.CoreExp a -> [Syn.CoreExp a] -> Syn.CoreExp a
app = anon Syn.app

soup :: [Syn.CoreExpr] -> Syn.CoreExpr
soup = anon Syn.soup

args :: [Syn.CoreExpr] -> Syn.CoreExpr
args = anon Syn.args

block :: [Syn.CoreExp a] -> Syn.CoreExp a
block = anon Syn.block

str :: String -> Syn.CoreExp a
str = anon Syn.str

sym :: String -> Syn.CoreExp a
sym = anon Syn.sym

withMeta :: Syn.CoreExp a -> Syn.CoreExp a -> Syn.CoreExp a
withMeta = anon Syn.withMeta

element :: String -> Syn.CoreExp a -> Syn.CoreExp a
element = anon Syn.element

corename :: Syn.CoreRelativeName -> Syn.CoreExp a
corename = anon Syn.corename

corelist :: [Syn.CoreExp a] -> Syn.CoreExp a
corelist = anon Syn.corelist

corenull :: Syn.CoreExp a
corenull = anon Syn.corenull

corebool :: Bool -> Syn.CoreExp a
corebool = anon Syn.corebool

corelookup
  :: Syn.CoreExp a -> Syn.CoreRelativeName -> Syn.CoreExp a
corelookup = anon Syn.corelookup

dynlookup ::
     Syn.CoreExp a -> Syn.CoreRelativeName -> Syn.CoreExp a -> Syn.CoreExp a
dynlookup = anon Syn.dynlookup
