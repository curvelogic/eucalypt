{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase, TemplateHaskell, DeriveTraversable, FlexibleContexts, FlexibleInstances, TupleSections #-}
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
import Bound.Scope
import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Data.Char (isDigit, isUpper)
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Data.List (isPrefixOf, elemIndex)
import Data.Maybe
import Data.Traversable (for)
import Eucalypt.Core.Anaphora
import Eucalypt.Core.SourceMap
import Safe (maximumMay)

-- | Primitive types (literals are available in the eucalypt syntax)
data Primitive
  = CoreInt Integer
  | CoreFloat Double
  | CoreString String
  | CoreSymbol String
  | CoreBoolean Bool
  | CoreNull
  deriving (Eq, Show, Read, Ord)



-- | A name in a bloc123k namespace, used in lookups
type CoreRelativeName = String



-- | A name used for a (free) binding
type CoreBindingName = String


-- | Type used to name intrinsics
type CoreBuiltinName = String


-- | Fixity of operator
data Fixity = UnaryPrefix | UnaryPostfix | InfixLeft | InfixRight
  deriving (Eq, Show, Read, Ord)

fixityArity :: Fixity -> Int
fixityArity UnaryPostfix = 1
fixityArity UnaryPrefix = 1
fixityArity _ = 2

-- | Precedence of operator
type Precedence = Int


-- | A marker to track whether a @CoreLet@ represents a block that
-- exposes all and only the bound names (i.e. the way we desugar block
-- AST). This allows us to apply some simplification (e.g. static
-- implementation of generalised lookup.)
data LetClass = DefaultBlockLet | OtherLet
  deriving (Eq, Show, Read, Ord)

-- | A new bound-based implementation, with multi-arity to allow STG
-- later.
--
data CoreExp a
  = CoreVar SMID a
    -- ^ variable
  | CoreLet SMID
            [(CoreBindingName, Scope Int CoreExp a)]
            (Scope Int CoreExp a)
            LetClass
    -- ^ names only for pretty-printing, not binding
  | CoreBuiltin SMID CoreBuiltinName
  | CorePrim SMID Primitive
    -- ^ literal
  | CoreLookup SMID
               (CoreExp a)
               CoreRelativeName
               (Maybe (CoreExp a))
    -- ^ from dot operator or name used in generalised lookup context
  | CoreName SMID CoreRelativeName
    -- ^ new parser - relative lookup name
  | CoreList SMID [CoreExp a]
  | CoreBlock SMID (CoreExp a)
  | CoreMeta SMID (CoreExp a) (CoreExp a)
  | CoreArgTuple SMID [CoreExp a]
    -- ^ RHS of call operator
  | CoreLambda SMID
               Bool
               [CoreBindingName]
               (Scope Int CoreExp a)
    -- ^ names for pretty-printing, not binding
  | CoreApply SMID
              (CoreExp a)
              [CoreExp a]
  | CoreOpSoup SMID [CoreExp a]
  | CoreOperator SMID
                 Fixity
                 Precedence
                 (CoreExp a)
  | CoreUnresolved SMID String
  | CoreRedeclaration SMID String
  | CoreEliminated
  deriving (Functor, Foldable, Traversable)


-- ? source map identifiers

sourceMapId :: CoreExp a -> SMID
sourceMapId (CoreVar smid _) = smid
sourceMapId (CoreLet smid _ _ _) = smid
sourceMapId (CoreBuiltin smid _) = smid
sourceMapId (CorePrim smid _) = smid
sourceMapId (CoreLookup smid _ _ _) = smid
sourceMapId (CoreName smid _) = smid
sourceMapId (CoreList smid _) = smid
sourceMapId (CoreBlock smid _) = smid
sourceMapId (CoreMeta smid _ _) = smid
sourceMapId (CoreArgTuple smid _) = smid
sourceMapId (CoreLambda smid _ _ _) = smid
sourceMapId (CoreApply smid _ _) = smid
sourceMapId (CoreOpSoup smid _) = smid
sourceMapId (CoreOperator smid _ _ _) = smid
sourceMapId (CoreUnresolved smid _) = smid
sourceMapId (CoreRedeclaration smid _) = smid
sourceMapId CoreEliminated = 0

mapSourceMapId :: CoreExp a -> (SMID -> SMID) -> CoreExp a
mapSourceMapId (CoreVar smid v) f = CoreVar (f smid) v
mapSourceMapId (CoreLet smid bs body cl) f = CoreLet (f smid) bs body cl
mapSourceMapId (CoreBuiltin smid n) f = CoreBuiltin (f smid) n
mapSourceMapId (CorePrim smid v) f = CorePrim (f smid) v
mapSourceMapId (CoreLookup smid o k d) f = CoreLookup (f smid) o k d
mapSourceMapId (CoreName smid n) f = CoreName (f smid) n
mapSourceMapId (CoreList smid xs) f = CoreList (f smid) xs
mapSourceMapId (CoreBlock smid l) f = CoreBlock (f smid) l
mapSourceMapId (CoreMeta smid m e) f = CoreMeta (f smid) m e
mapSourceMapId (CoreArgTuple smid xs) f = CoreArgTuple (f smid) xs
mapSourceMapId (CoreLambda smid i ns body) f = CoreLambda (f smid) i ns body
mapSourceMapId (CoreApply smid fn xs) f = CoreApply (f smid) fn xs
mapSourceMapId (CoreOpSoup smid xs) f = CoreOpSoup (f smid) xs
mapSourceMapId (CoreOperator smid x p e) f = CoreOperator (f smid) x p e
mapSourceMapId (CoreUnresolved smid v) f = CoreUnresolved (f smid) v
mapSourceMapId (CoreRedeclaration smid v) f = CoreRedeclaration (f smid) v
mapSourceMapId CoreEliminated _ = CoreEliminated

fillSourceMapId :: CoreExp a -> SMID -> CoreExp a
fillSourceMapId expr smid = mapSourceMapId expr (\i -> if i == 0 then smid else i)

instance HasSourceMapIds (CoreExp a) where
  toSourceMapIds e = [sourceMapId e]

-- | Core expression using a simple string binding name
type CoreExpr = CoreExp CoreBindingName

-- | True if expression is a block
isBlock :: CoreExp a -> Bool
isBlock CoreBlock{} = True
isBlock _ = False



-- | True if expression is a list
isList :: CoreExp a -> Bool
isList CoreList{} = True
isList _ = False



-- | Return the name of a symbol if the expression is a symbol.
symbolName :: CoreExp a -> Maybe String
symbolName (CorePrim _ (CoreSymbol s)) = Just s
symbolName _ = Nothing



-- | String content of string literal if the expression is a string.
stringContent :: CoreExp a -> Maybe String
stringContent (CorePrim _ (CoreString s)) = Just s
stringContent _ = Nothing



isEliminated :: CoreExp a -> Bool
isEliminated CoreEliminated = True
isEliminated _ = False




deriveEq1   ''CoreExp
deriveOrd1  ''CoreExp
deriveRead1 ''CoreExp
deriveShow1 ''CoreExp
instance Eq a => Eq (CoreExp a) where (==) = eq1
instance Ord a => Ord (CoreExp a) where compare = compare1
instance Show a => Show (CoreExp a) where showsPrec = showsPrec1
instance Read a => Read (CoreExp a) where readsPrec = readsPrec1



instance Applicative CoreExp where
  pure = CoreVar 0
  (<*>) = ap



instance Monad CoreExp where
  return = CoreVar 0
  CoreVar smid a >>= f = fillSourceMapId (f a) smid
  CoreLet smid bs b cl >>= f = CoreLet smid (map (second (>>>= f)) bs) (b >>>= f) cl
  CoreBuiltin smid n >>= _ = CoreBuiltin smid n
  CorePrim smid p >>= _ = CorePrim smid p
  CoreLookup smid e n d >>= f = CoreLookup smid (e >>= f) n ((>>= f) <$> d)
  CoreList smid es >>= f = CoreList smid (map (>>= f) es)
  CoreBlock smid e >>= f = CoreBlock smid (e >>= f)
  CoreMeta smid m e >>= f = CoreMeta smid (m >>= f) (e >>= f) -- TODO: separate meta?
  CoreOpSoup smid es >>= f = CoreOpSoup smid (map (>>= f) es)
  CoreLambda smid i n e >>= f = CoreLambda smid i n (e >>>= f)
  CoreOperator smid x p e >>= f = CoreOperator smid x p (e >>= f)
  CoreArgTuple smid es >>= f = CoreArgTuple smid (map (>>= f) es)
  CoreApply smid g es >>= f = CoreApply smid (g >>= f) (map (>>= f) es)
  CoreName smid n >>= _ = CoreName smid n
  CoreUnresolved smid v >>= _ = CoreUnresolved smid v
  CoreRedeclaration smid v >>= _ = CoreRedeclaration smid v
  CoreEliminated >>= _ = CoreEliminated



-- | Construct a var
var :: SMID -> a -> CoreExp a
var = CoreVar



-- | Construct a name (maybe be a relative name, not a var)
corename :: SMID -> CoreRelativeName -> CoreExp a
corename = CoreName



-- | Construct a lookup
corelookup :: SMID -> CoreExp a -> CoreRelativeName -> CoreExp a
corelookup smid obj n = CoreLookup smid obj n Nothing



-- | Construct a lookup with default (such as might result from using
-- a name in a dynamic generalised lookup context.)
dynlookup :: SMID -> CoreExp a -> CoreRelativeName -> CoreExp a -> CoreExp a
dynlookup smid obj n def = CoreLookup smid obj n (Just def)



-- | Abstract lambda of several args
lam :: SMID -> [CoreBindingName] -> CoreExpr -> CoreExpr
lam smid as expr = CoreLambda smid False as scope
  where
    scope = abstract (`elemIndex` as) expr



-- | Construct a function application
app :: SMID -> CoreExp a -> [CoreExp a] -> CoreExp a
app = CoreApply



-- | Construct recursive let of several bindings
letexp :: SMID -> [(CoreBindingName, CoreExpr)] -> CoreExpr -> CoreExpr
letexp _ [] b = b
letexp smid bs b = CoreLet smid (map (second abstr) bs) (abstr b) OtherLet
  where
    abstr = abstract (`elemIndex` map fst bs)



-- | Construct a recursive let that records it is a @DefaultBlockLet@
letblock :: SMID -> [(CoreBindingName, CoreExpr)] -> CoreExpr -> CoreExpr
letblock smid bs b =
  CoreLet smid (map (second abstr) bs) (abstr b) DefaultBlockLet
  where
    abstr = abstract (`elemIndex` map fst bs)



-- | Construct boolean expression
corebool :: SMID -> Bool -> CoreExp a
corebool smid = CorePrim smid . CoreBoolean



-- | Construct null expression
corenull :: SMID -> CoreExp a
corenull smid = CorePrim smid CoreNull



-- | CoreList
corelist :: SMID -> [CoreExp a] -> CoreExp a
corelist = CoreList



-- | Construct symbol expression
sym :: SMID -> String -> CoreExp a
sym smid = CorePrim smid . CoreSymbol



-- | Construct builtin expression
bif :: SMID -> String -> CoreExp a
bif = CoreBuiltin



-- | Construct an integer expression
int :: SMID -> Integer -> CoreExp a
int smid = CorePrim smid . CoreInt



-- | Construct an integer expression
float :: SMID -> Double -> CoreExp a
float smid = CorePrim smid . CoreFloat



-- | Construct a string expression
str :: SMID -> String -> CoreExp a
str smid = CorePrim smid . CoreString



-- | A block element from string key and expr value
element :: SMID -> String -> CoreExp a -> CoreExp a
element smid k v = CoreList smid [anon sym k, v]



-- | A block from its items
block :: SMID -> [CoreExp a] -> CoreExp a
block smid items = CoreBlock smid $ CoreList smid items



-- | Apply metadata to another expression
withMeta :: SMID -> CoreExp a -> CoreExp a -> CoreExp a
withMeta = CoreMeta



-- | A left-associative infix operation
infixl_ :: SMID -> Precedence -> CoreExp a -> CoreExp a
infixl_ smid = CoreOperator smid InfixLeft



-- | A right-associative infix operation
infixr_ :: SMID -> Precedence -> CoreExp a -> CoreExp a
infixr_ smid = CoreOperator smid InfixRight



-- | A unary prefix operator
prefix_ :: SMID -> Precedence -> CoreExpr -> CoreExpr
prefix_ smid = CoreOperator smid UnaryPrefix



-- | A unary postfix operat
postfix_ :: SMID -> Precedence -> CoreExpr -> CoreExpr
postfix_ smid = CoreOperator smid UnaryPostfix



-- | Operator soup without explicit brackets
soup :: SMID -> [CoreExpr] -> CoreExpr
soup = CoreOpSoup



-- | Arg tuple constructor
args :: SMID -> [CoreExpr] -> CoreExpr
args = CoreArgTuple


-- | Unresolved marker
unresolved :: SMID -> String -> CoreExpr
unresolved = CoreUnresolved


-- $ special operators
--


-- | Catenation operator
catOp :: CoreExp a
catOp = anon infixl_ 20 (anon bif "CAT")



-- | Function calls using arg tuple are treated as operator during the
-- fixity / precedence resolution phases but formed into core syntax
-- after that.
callOp :: CoreExp a
callOp = anon infixl_ 90 (anon bif "*CALL*")



-- | Name lookup is treated as operator during the fixity / precedence
-- resolution phases but formed into core syntax after that.
lookupOp :: CoreExpr
lookupOp = anon infixl_ 90 (anon bif "*DOT*")


-- | Names that aren't in lookup positions need to become variables or
-- builtins as appropriate
name2Var :: SMID -> String -> CoreExpr
name2Var smid n
  | "__" `isPrefixOf` n && isUpper (n !! 2) = CoreBuiltin smid (drop 2 n)
  | otherwise = CoreVar smid n



-- | In contexts where single names should become variables (evaluand
-- rather than individual lookup elements for instance), this converts
-- to vars.
varify :: CoreExpr -> CoreExpr
varify (CoreName smid n) = name2Var smid n
varify e = e


-- ? anaphora
--
-- These are instances and functions for handling expression anaphora
-- (e.g '_', '_0', '_1',...) which implicitly define lambdas by
-- referring to automatic parameters

-- An anaphora type which is introduced by a single special character
-- with optional number
newtype SymbolicAnaphora = SymbolicAnaphora Char

-- | SymbolicAnaphora can be applied with Strings (CoreBindingNames)
instance Anaphora SymbolicAnaphora CoreBindingName where
  unnumberedAnaphor (SymbolicAnaphora c) = [c]

  isAnaphor (SymbolicAnaphora c) s
    | s == [c] = True
    | (isJust . anaphorIndex c) s = True
  isAnaphor _ _ = False

  toNumber (SymbolicAnaphora c) = anaphorIndex c

  fromNumber (SymbolicAnaphora c) n = c : show n

  toName _ = id

-- | SymbolicAnaphora can be applied with 'Var b CoreBindingName'
instance (Anaphora t a, Eq b, Show b) => Anaphora t (Var b a) where
  unnumberedAnaphor t = F $ unnumberedAnaphor t

  isAnaphor t (F s)
    | isAnaphor t s = True
    | (isJust . toNumber t) s = True
  isAnaphor _ _ = False

  toNumber t (F s) = toNumber t s
  toNumber _ _ = Nothing

  fromNumber t n = F $ fromNumber t n

  toName t (F s) = toName t s
  toName _ (B _) = ""


-- | Is it the name of a numbered symbolic anaphoric parameter? @_@
-- doesn't count as it should have been substituted for a numbered
-- version by cooking.
anaphorIndex :: Char -> String -> Maybe Int
anaphorIndex c (c0:s:_) | c == c0 && isDigit s  = Just (read [s] :: Int)
anaphorIndex _ _ = Nothing

-- | Expression anaphora are '_', '_0', '_1', ...
expressionAnaphora :: SymbolicAnaphora
expressionAnaphora = SymbolicAnaphora '_'

-- | Block anaphora are '•', '•0', '•1', ...
blockAnaphora :: SymbolicAnaphora
blockAnaphora = SymbolicAnaphora '•'

-- | The unnumbered anaphor as an expression
expressionAnaphor :: CoreExpr
expressionAnaphor = anon var (unnumberedAnaphor expressionAnaphora)

-- | Is the expression an anaphoric var according to the anaphor type
-- provided?
isAnaphoricVar :: (Anaphora t a) => t -> CoreExp a -> Bool
isAnaphoricVar t (CoreVar _ s) = isAnaphor t s
isAnaphoricVar _ _ = False

-- | Apply a number to the unnumbered anaphor
applyNumber :: (Anaphora t a) => t -> a -> State Int a
applyNumber t s | s == unnumberedAnaphor t = do
  n <- get
  put (n + 1)
  return $ fromNumber t n
applyNumber _ x = return x

-- | Add numbers to numberless anaphora
numberAnaphora :: Anaphora t a => t -> CoreExp a -> CoreExp a
numberAnaphora t expr = flip evalState (0 :: Int) $ for expr (applyNumber t)



-- | Bind anaphora
--
-- Wrap a lambda around the expression, binding all anaphoric
-- parameters unless there are none in which case return the
-- expression unchanged
bindAnaphora :: forall a. forall t.
     Anaphora t a => t -> CoreExp a -> CoreExp a
bindAnaphora t expr =
  case maxAnaphor of
    Just n ->
      CoreLambda (sourceMapId expr) False (anaphora n) $
      abstract (toNumber t) expr
    Nothing -> expr
  where
    freeVars = foldr (:) [] expr
    freeAnaphora = mapMaybe (toNumber t) freeVars
    maxAnaphor = maximumMay freeAnaphora
    anaphora upTo = map ((toName t :: a -> String) . (fromNumber t :: Int -> a)) [0 .. upTo]



-- $ substitutions
--
-- The crude bootstrap interpreter just manipulates core expressions.
-- We keep the abstraction and instantiation workings (using the Bound
-- library) in here.
--

-- | Instantiate arguments into lambda body (new multi-arg lambda)
--
instantiateBody :: [CoreExpr] -> Scope Int CoreExp CoreBindingName -> CoreExpr
instantiateBody vals = instantiate (vals !!)

-- | Use Bound to wire the bindings into the appropriate places in the
-- expressions (lazily...). This expands using bindings in top-level
-- lets
instantiateLet :: CoreExp a -> CoreExp a
instantiateLet (CoreLet _ bs b _) = inst b
  where
    es = map (inst . snd) bs
    inst = instantiate (es !!)
instantiateLet expr = expr


-- ? rebodying

-- | Allows us to retrieve the CoreBindingName of a variable even when
-- it is wrapped in several layers of 'Var'
class ToCoreBindingName c where
  toCoreBindingName :: c -> Maybe CoreBindingName

instance ToCoreBindingName String where
  toCoreBindingName = Just

instance ToCoreBindingName a => ToCoreBindingName (Var b a) where
  toCoreBindingName v = case v of
    F a -> toCoreBindingName a
    B _ -> Nothing


-- | Navigates down through lets and transparent expressions (like
-- metadata and traces) and replaces the innermost body, binding free
-- expressions in that body according to the bindings of the
-- containing lets.
rebody :: (ToCoreBindingName a, Show a) => CoreExp a -> CoreExp a -> CoreExp a
rebody (CoreLet smid bs body _) payload =
  let payload' = rebody (fromScope body) (fmap return payload)
   in CoreLet smid bs (bindMore'' toNameAndBinding (toScope payload')) OtherLet
  where
    toNameAndBinding :: ToCoreBindingName a => a -> Maybe (CoreBindingName, Int)
    toNameAndBinding nm =
      case toCoreBindingName nm of
        (Just b) -> (b, ) <$> (b `elemIndex` map fst bs)
        Nothing -> Nothing
rebody (CoreMeta smid m e) payload = CoreMeta smid m (rebody e payload)
rebody _ payload = payload


-- | For binding further free variables in an expression that has
-- already been abstracted once and is therefore a Scope.
bindMore'' ::
     Monad f
  => (a -> Maybe (CoreBindingName, b))
  -> Scope b f a
  -> Scope b f a
bindMore'' k = toScope . bindFree . fromScope
  where
    bindFree e =
      e >>= \v ->
        return $
        case v of
          F a -> bind a
          B b -> B b
    bind a =
      case k a of
        (Just (_, z)) -> B z
        Nothing -> F a



-- | For binding further free variables in an expression that has
-- already been abstracted once and is therefore a Scope.
bindMore ::
     Monad f => (a -> Maybe b) -> Scope b f a -> Scope b f a
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
        Just z -> B  z
        Nothing -> F a



-- | Modify (e.g. wrap) bound variables as specified by the
-- transformation function 'k'.
modifyBoundVars ::
     Monad f
  => (b -> f (Var b (f a)) -> f (Var b (f a)))
  -> Scope b f a
  -> Scope b f a
modifyBoundVars k e =
  Scope $
  unscope e >>= \case
      B b ->
        let f = k b
         in f (pure (B b))
      F a -> pure $ F a



-- | Replace bound variables in let and lambda bodies with
-- appropriately named free variables. Handy for inspecting
-- expression in tests.
unbind :: CoreExpr -> CoreExpr
unbind (CoreLambda _ _ _ e) = instantiate (anon var . show) e
unbind (CoreLet _ bs body _) = inst body
  where
    names = map fst bs
    inst = instantiate (\n -> anon var (names !! n))
unbind e = e



-- | Pull a unit (let or block) apart into bindings and body
unitBindingsAndBody ::
  CoreExpr
  -> ([(CoreBindingName, Scope Int CoreExp CoreBindingName)],
      Scope Int CoreExp CoreBindingName)
unitBindingsAndBody (CoreMeta _ _ b) = unitBindingsAndBody b
unitBindingsAndBody (CoreLet _ bs b _) = (bs, b)
unitBindingsAndBody e@CoreBlock{} = ([], abstract (const Nothing) e)
unitBindingsAndBody CoreList{} = error "Input is a sequence and must be named."
unitBindingsAndBody _ = error "Unsupported unit type (not block or sequence)"



-- | Merge bindings from core units, using body of final unit as
-- default body.
--
mergeUnits :: [CoreExpr] -> CoreExpr
mergeUnits lets = last newLets
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
          shift = mapBound (\i -> i + length nextBindings)
          reboundNextBindings = map (second abstr) nextBindings
          shiftedEstablishedBindings = map (second shift) establishedBindings
       in reboundNextBindings ++ shiftedEstablishedBindings
    rebindBody oldBody newBindList =
      let abstr = bindMore (`elemIndex` map fst newBindList)
       in abstr oldBody
    newLets = zipWith makeLet bindLists' bodies'
    makeLet = anon (\s bs b -> CoreLet s bs b OtherLet)
