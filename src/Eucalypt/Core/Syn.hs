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

-- | Primitive types (literals are available in the eucalypt syntax)
data Primitive
  = Int Integer
  | Float Double
  | String String
  | Symbol String
  deriving (Eq, Show)

newtype Name
  = Local String -- TODO: -> de bruijn
  deriving (Eq, Show)

localName :: Name -> String
localName (Local n) = n

class FromString a where
  fromStr :: String -> a

instance FromString Name where
  fromStr = Local

-- | Expression in the core syntax. ` is binder type
data CoreExpr a
  = Lam a (CoreExpr a)
  | App (CoreExpr a) (CoreExpr a)
  | Var a
  | Prim Primitive
  | Let Bool [(a, CoreExpr a)] (CoreExpr a)
  | Lookup (CoreExpr a) Name
  | BlockValue [(CoreExpr a, CoreExpr a)]
  | ListValue [CoreExpr a]
  deriving (Eq, Show)

-- | Construct a (possibly multi-arg) lambda
lam :: [a] -> CoreExpr a -> CoreExpr a
lam args expr = foldr Lam expr args

-- | Construct a function application
app :: CoreExpr a -> [CoreExpr a] -> CoreExpr a
app = foldl App

-- | Construct recursive let
letrec :: [(a, CoreExpr a)] -> CoreExpr a -> CoreExpr a
letrec = Let True

-- Transformation from AST to Core

-- | InitialCoreExpr is type of the initial Core representation when
-- | first transformed from the AST.
type InitialCoreExpr = CoreExpr Name
