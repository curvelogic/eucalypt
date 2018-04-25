{-# LANGUAGE TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-|
Module      : Eucalypt.Core.Syn
Description : Core expression forms for the Eucalypt language
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Syn
where

import Bound
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Data.List (elemIndex)
import Control.Monad



-- | Primitive types (literals are available in the eucalypt syntax)
data Primitive
  = CoreInt Integer
  | CoreFloat Double
  | CoreString String
  | CoreSymbol String
  | CoreBoolean Bool
  | CoreNull
  deriving (Eq, Show, Read, Ord)



-- | A name in a block namespace, used in lookups
type CoreRelativeName = String



-- | A name used for a (free) binding
type CoreBindingName = String



type CoreBuiltinName = String

-- | A new bound-based implementation, with multi-arity to allow STG
-- later.
--
data CoreExp a
  = CoreVar a
  | CoreLam (Scope () CoreExp a)
  | CoreLet [Scope Int CoreExp a] (Scope Int CoreExp a)
  | CoreApp (CoreExp a) (CoreExp a)
  | CoreBuiltin CoreBuiltinName
  | CorePAp Int (CoreExp a) [CoreExp a]
  | CorePrim Primitive
  | CoreLookup (CoreExp a) CoreRelativeName
  | CoreList [CoreExp a]
  | CoreBlock (CoreExp a)
  deriving (Functor,Foldable,Traversable)



deriveEq1   ''CoreExp
deriveOrd1  ''CoreExp
deriveRead1 ''CoreExp
deriveShow1 ''CoreExp
instance Eq a => Eq (CoreExp a) where (==) = eq1
instance Ord a => Ord (CoreExp a) where compare = compare1
instance Show a => Show (CoreExp a) where showsPrec = showsPrec1
instance Read a => Read (CoreExp a) where readsPrec = readsPrec1



instance Applicative CoreExp where
  pure = CoreVar
  (<*>) = ap



instance Monad CoreExp where
  return = CoreVar
  CoreVar a >>= f = f a
  CoreLam e >>= f = CoreLam (e >>>= f)
  CoreLet bs b >>= f = CoreLet (map (>>>= f) bs) (b >>>= f)
  CoreBuiltin n >>= _ = CoreBuiltin n
  CoreApp g a >>= f = CoreApp (g >>= f) (a >>= f)
  CorePAp a e args >>= f = CorePAp a (e >>= f) (map (>>= f) args)
  CorePrim p >>= _ = CorePrim p
  CoreLookup e n >>= f = CoreLookup (e >>= f) n
  CoreList es >>= f = CoreList (map (>>= f) es)
  CoreBlock e >>= f = CoreBlock (e >>= f)



-- | Abstract a lambda into a scope
lamexp :: Eq a => a -> CoreExp a -> CoreExp a
lamexp x b = CoreLam (abstract1 x b)



-- | Abstract lambda of several args
lamexpr :: Eq a => [a] -> CoreExp a -> CoreExp a
lamexpr args expr = foldr lamexp expr args



-- | Construct a function application
appexp :: CoreExp a -> [CoreExp a] -> CoreExp a
appexp = foldl CoreApp



-- | Construct recursive let of several bindings
letexp :: Eq a => [(a, CoreExp a)] -> CoreExp a -> CoreExp a
letexp [] b = b
letexp bs b = CoreLet (map (abstr . snd) bs) (abstr b)
  where abstr = abstract (`elemIndex` map fst bs)



-- | Core expression using a simple string binding name
type CoreExpr = CoreExp CoreBindingName
