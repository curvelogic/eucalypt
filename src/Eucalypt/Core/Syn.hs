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

import Debug.Trace
import Bound
import Bound.Scope
import Bound.Name
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Data.List (elemIndex)
import Control.Monad
import Data.Bifunctor (second)


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
  | CoreLam (Scope (Name String ()) CoreExp a)
  | CoreLet [(CoreBindingName, Scope (Name String Int) CoreExp a)] (Scope (Name String Int) CoreExp a)
  | CoreApp (CoreExp a) (CoreExp a)
  | CoreBuiltin CoreBuiltinName
  | CorePAp Int (CoreExp a) [CoreExp a]
  | CorePrim Primitive
  | CoreLookup (CoreExp a) CoreRelativeName
  | CoreList [CoreExp a]
  | CoreBlock (CoreExp a)
  | CoreMeta (CoreExp a) (CoreExp a)
  deriving (Functor,Foldable,Traversable)


-- | Core expression using a simple string binding name
type CoreExpr = CoreExp CoreBindingName

-- | True if expression is a block
isBlock :: CoreExp a -> Bool
isBlock (CoreBlock _) = True
isBlock _ = False


-- | True if expression is a list
isList :: CoreExp a -> Bool
isList (CoreList _) = True
isList _ = False


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
  CoreLet bs b >>= f = CoreLet (map (second (>>>= f)) bs) (b >>>= f)
  CoreBuiltin n >>= _ = CoreBuiltin n
  CoreApp g a >>= f = CoreApp (g >>= f) (a >>= f)
  CorePAp a e args >>= f = CorePAp a (e >>= f) (map (>>= f) args)
  CorePrim p >>= _ = CorePrim p
  CoreLookup e n >>= f = CoreLookup (e >>= f) n
  CoreList es >>= f = CoreList (map (>>= f) es)
  CoreBlock e >>= f = CoreBlock (e >>= f)
  CoreMeta m e >>= f = CoreMeta (m >>= f) (e >>= f)



-- | Construct a var
var :: a -> CoreExp a
var = CoreVar



-- | Abstract a lambda into a scope
lamexp :: CoreBindingName -> CoreExpr -> CoreExpr
lamexp x b = CoreLam (abstract1Name x b)



-- | Abstract lambda of several args
lamexpr :: [CoreBindingName] -> CoreExpr -> CoreExpr
lamexpr args expr = foldr lamexp expr args



-- | Construct a function application
appexp :: CoreExp a -> [CoreExp a] -> CoreExp a
appexp = foldl CoreApp



-- | Construct recursive let of several bindings
letexp :: [(CoreBindingName, CoreExpr)] -> CoreExpr -> CoreExpr
letexp [] b = b
letexp bs b = CoreLet (map (second abstr) bs) (abstr b)
  where abstr = abstractName (`elemIndex` map fst bs)



-- | Construct boolean expression
corebool :: Bool -> CoreExp a
corebool = CorePrim . CoreBoolean



-- | Construct null expression
corenull :: CoreExp a
corenull = CorePrim CoreNull



-- | Construct symbol expression
sym :: String -> CoreExp a
sym = CorePrim . CoreSymbol



-- | Construct builtin expression
bif :: String -> CoreExp a
bif = CoreBuiltin



-- | Construct an integer expression
int :: Integer -> CoreExp a
int = CorePrim . CoreInt



-- | Construct an integer expression
float :: Double -> CoreExp a
float = CorePrim . CoreFloat



-- | Construct a string expression
str :: String -> CoreExp a
str = CorePrim . CoreString



-- | A block element from string key and expr value
element :: String -> CoreExp a -> CoreExp a
element k v = CoreList [sym k, v]



-- | A block from its items
block :: [CoreExpr] -> CoreExpr
block items = CoreBlock $ CoreList items



-- $ substitutions
--
-- The crude bootstrap interpreter just manipulates core expressions.
-- We keep the abstraction and instantiation workings (using the Bound
-- library) in here.
--


-- | Use Bound to wire a value into a lambda body
--
instantiateLambda :: CoreExpr -> CoreExpr -> CoreExpr
instantiateLambda val (CoreLam body) = instantiate1Name val body
instantiateLambda _ _ = undefined



-- | Use Bound to wire the bindings into the appropriate places in the
-- expressions (lazily...)
instantiateLet :: CoreExpr -> CoreExpr
instantiateLet (CoreLet bs b) = inst b
  where
    es = map (inst . snd) bs
    inst = instantiateName (es !!)
instantiateLet _ = undefined



-- | For binding further free variables in an expression that has
-- already been abstracted once and is therefore a Scope.
bindMore ::
     Monad f => (a -> Maybe b) -> Scope (Name a b) f a -> Scope (Name a b) f a
bindMore k = toScope . bindFree . fromScope
  where
    bindFree e =
      e >>= \v ->
        return $
        case v of
          F a -> bind a
          B b -> B b
    bind a =
      case k a of
        Just z -> B (Name a z)
        Nothing -> F a



-- | Replace bound variables in let and lambda bodies with
-- appropriately named free variables. Handy for inspecting
-- expression in tests.
unbind :: CoreExpr -> CoreExpr
unbind (CoreLam e) = instantiate1Name (CoreVar n) e
  where
    n = foldMapBound name e
unbind (CoreLet bs body) = inst body
  where
    names = map fst bs
    inst = instantiateName (\n -> CoreVar (names !! n))
unbind e = e


-- | Pull a unit (let or block) apart into bindings and body
unitBindingsAndBody ::
  CoreExpr
  -> ([(CoreBindingName, Scope (Name String Int) CoreExp CoreBindingName)],
      Scope (Name String Int) CoreExp CoreBindingName)
unitBindingsAndBody (CoreLet bs b) = (bs, b)
unitBindingsAndBody e@CoreBlock{} = ([], abstractName (const Nothing) e)
unitBindingsAndBody e = trace (show e) $ error "not a let"


-- | Merge core units together, binding free variables in later units
-- to values supplied by earlier units.
--
mergeUnits :: [CoreExpr] -> CoreExpr
mergeUnits lets = foldl1 (flip CoreApp) newLets
  where
    (bindLists, bodies) = unzip (map unitBindingsAndBody lets)
    bindLists' = scanl1 rebindBindings bindLists
    bodies' = zipWith rebindBody bodies bindLists'
    rebindBindings establishedBindings nextBindings =
      let abstr =
            bindMore
              (\n ->
                 (length nextBindings +) <$>
                 (n `elemIndex` map fst establishedBindings))
       in let reboundNextBindings = map (second abstr) nextBindings
           in reboundNextBindings ++ establishedBindings
    rebindBody oldBody newBindList =
      let abstr = bindMore (`elemIndex` map fst newBindList)
       in abstr oldBody
    newLets = zipWith CoreLet bindLists' bodies'
