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


-- | Type used to name intrinsics
type CoreBuiltinName = String


-- | Fixity of operator
data Fixity = UnaryPrefix | UnaryPostfix | InfixLeft | InfixRight
  deriving (Eq, Show, Read, Ord)

-- | Precedence of operator
type Precedence = Int

-- | A new bound-based implementation, with multi-arity to allow STG
-- later.
--
data CoreExp a
  = CoreVar a
  | CoreLet [(CoreBindingName, Scope (Name String Int) CoreExp a)] (Scope (Name String Int) CoreExp a)
  | CoreBuiltin CoreBuiltinName
  | CorePrim Primitive
  | CoreLookup (CoreExp a) CoreRelativeName
  | CoreList [CoreExp a]
  | CoreBlock (CoreExp a)
  | CoreMeta (CoreExp a) (CoreExp a)
  | CoreName CoreRelativeName -- ^ new parser - relative lookup name
  | CoreArgTuple [CoreExp a] -- ^ new parser
  | CoreLambda Int (Scope (Name String Int) CoreExp a) -- ^ new parser
  | CoreApply (CoreExp a) [CoreExp a] -- ^ new parser
  | CoreOpSoup [CoreExp a] -- ^ new parser
  | CoreOperator Fixity Precedence (CoreExp a) -- ^ new parser
  | CorePAp Int (CoreExp a) [CoreExp a] -- ^ during evaluation only
  | CoreTraced (CoreExp a) -- ^ during evaluation only
  | CoreChecked (CoreExp a) (CoreExp a) -- ^ during evaluation only
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


-- | Return the name of a symbol if the expression is a symbol.
symbolName :: CoreExpr -> Maybe String
symbolName (CorePrim (CoreSymbol s)) = Just s
symbolName _ = Nothing



-- | String content of string literal if the expression is a string.
stringContent :: CoreExpr -> Maybe String
stringContent (CorePrim (CoreString s)) = Just s
stringContent _ = Nothing



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
  CoreLet bs b >>= f = CoreLet (map (second (>>>= f)) bs) (b >>>= f)
  CoreBuiltin n >>= _ = CoreBuiltin n
  CorePAp a e as >>= f = CorePAp a (e >>= f) (map (>>= f) as)
  CorePrim p >>= _ = CorePrim p
  CoreLookup e n >>= f = CoreLookup (e >>= f) n
  CoreList es >>= f = CoreList (map (>>= f) es)
  CoreBlock e >>= f = CoreBlock (e >>= f)
  CoreMeta m e >>= f = CoreMeta (m >>= f) (e >>= f) -- TODO: separate meta?
  CoreTraced e >>= f = CoreTraced (e >>= f)
  CoreChecked check e >>= f = CoreChecked (check >>= f) (e >>= f)
  CoreOpSoup es >>= f = CoreOpSoup (map (>>= f) es)
  CoreLambda n e >>= f = CoreLambda n (e >>>= f)
  CoreOperator x p e >>= f = CoreOperator x p (e >>= f)
  CoreArgTuple es >>= f = CoreArgTuple (map (>>= f) es)
  CoreApply g es >>= f = CoreApply (g >>= f) (map (>>= f) es)
  CoreName n >>= _ = CoreName n

-- | Construct a var
var :: a -> CoreExp a
var = CoreVar



-- | Construct a name (maybe be a relative name, not a var)
corename :: CoreRelativeName -> CoreExpr
corename = CoreName



-- | Abstract lambda of several args
lam :: [CoreBindingName] -> CoreExpr -> CoreExpr
lam as expr = CoreLambda (length as) scope
  where scope = abstractName (`elemIndex` as) expr



--- | Construct a function application
app :: CoreExp a -> [CoreExp a] -> CoreExp a
app = CoreApply



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



-- | A left-associative infix operation
infixl_ :: Precedence -> CoreExpr -> CoreExpr
infixl_ = CoreOperator InfixLeft



-- | A right-associative infix operation
infixr_ :: Precedence -> CoreExpr -> CoreExpr
infixr_ = CoreOperator InfixRight



-- | A unary prefix operator
prefix_ :: Precedence -> CoreExpr -> CoreExpr
prefix_ = CoreOperator UnaryPrefix



-- | A unary postfix operat
postfix_ :: Precedence -> CoreExpr -> CoreExpr
postfix_ = CoreOperator UnaryPostfix



-- | Operator soup without explicit brackets
soup :: [CoreExpr] -> CoreExpr
soup = CoreOpSoup



-- | Arg tuple constructor
args :: [CoreExpr] -> CoreExpr
args = CoreArgTuple

-- $ special operators
--


-- | Catenation operator
catOp :: CoreExpr
catOp = infixl_ 20 (CoreBuiltin "CAT")


-- | Function calls using arg tuple are treated as operator during the
-- fixity / precedence resolution phases but formed into core syntax
-- after that.
callOp :: CoreExpr
callOp = infixl_ 90 (CoreBuiltin "*CALL*")



-- | Name lookup is treated as operator during the fixity / precedence
-- resolution phases but formed into core syntax after that.
lookupOp :: CoreExpr
lookupOp = infixl_ 95 (CoreBuiltin "*DOT*")



-- $ substitutions
--
-- The crude bootstrap interpreter just manipulates core expressions.
-- We keep the abstraction and instantiation workings (using the Bound
-- library) in here.
--

-- | Instantiate single argument into lambda body (old lambda)
--
instantiate1Body :: CoreExpr -> Scope (Name String ()) CoreExp CoreBindingName -> CoreExpr
instantiate1Body = instantiate1Name

-- | Instantiate arguments into lambda body (new multi-arg lambda)
--
instantiateBody :: [CoreExpr] -> Scope (Name String Int) CoreExp CoreBindingName -> CoreExpr
instantiateBody vals = instantiateName (vals !!)

-- | Use Bound to wire the bindings into the appropriate places in the
-- expressions (lazily...)
instantiateLet :: CoreExpr -> CoreExpr
instantiateLet (CoreLet bs b) = inst b
  where
    es = map (inst . snd) bs
    inst = instantiateName (es !!)
instantiateLet _ = error "instantiateLet called on non-let"



-- | Turn a block into let bindings, allowing the values to be bound
-- to by variables in body. Metadata from the block annotations is
-- rebound to the bound values as value metadata.
abstractStaticBlock :: CoreExpr -> CoreExpr -> CoreExpr
abstractStaticBlock (CoreBlock (CoreList l)) body =
  CoreLet (map (second abstr) bs) (abstr body)
  where
    bs = map binding l
    binding (CoreMeta m (CoreList [CorePrim (CoreSymbol k), v])) = (k, CoreMeta m v)
    binding (CoreList [CorePrim (CoreSymbol k), v]) = (k, v)
    binding _ = error "Unexpected binding item in abstractStaticBlock"
    abstr = abstractName (`elemIndex` map fst bs)
abstractStaticBlock _ _ = error "abstractStaticBlock called on non-block"



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
unbind (CoreLambda _ e) = instantiateName (CoreVar . show) e
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
mergeUnits lets = foldl1 merge newLets
  where
    merge a b = CoreApply b [a]
    (bindLists, bodies) = unzip (map unitBindingsAndBody lets)
    bindLists' = scanl1 rebindBindings bindLists
    bodies' = zipWith rebindBody bodies bindLists'
    rebindBindings establishedBindings nextBindings =
      let abstr =
            bindMore
              (\n ->
                 (length nextBindings +) <$>
                 (n `elemIndex` map fst establishedBindings))
          shift = mapBound (\(Name n i) -> (Name n (i + length nextBindings)))
          reboundNextBindings = map (second abstr) nextBindings
          shiftedEstablishedBindings = map (second shift) establishedBindings
      in reboundNextBindings ++ shiftedEstablishedBindings
    rebindBody oldBody newBindList =
      let abstr = bindMore (`elemIndex` map fst newBindList)
       in abstr oldBody
    newLets = zipWith CoreLet bindLists' bodies'
