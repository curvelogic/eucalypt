{-|
Module      : Eucalypt.Stg.Compiler.Context
Description : Tracking name references during compilation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Compiler.Context where

import Bound.Var
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Eucalypt.Stg.Syn

-- | Map of names to refs into the environment
type Context v = v -> Ref

emptyContext :: Context v
emptyContext _ = error "Not found in context"

-- | Create context that returns local indexes according to the order
-- of variables provided.
toContext :: Eq v => [v] -> Context v
toContext vs v = (L . fromIntegral) $ fromJust (v `elemIndex` vs)

-- | From a context for the surrounding environment, create a context
-- for a lambda form that can also resolve bound variables to local
-- references.
lambdaContext :: Int -> Context v -> Context (Var Int v)
lambdaContext _ freeContext (F v) = freeContext v
lambdaContext freeSize _ (B i) = L $ fromIntegral $ freeSize + i

-- | When we push an expression inside a PreClosure, its references to
-- variables should now go via indexes into the PreClosure's
-- free refereces. This provides a context which maps the references
-- as appropriate.
closureContext :: [Ref] -> Context v -> Context v
closureContext env context v =
  let orig = context v
   in case orig `elemIndex` env of
        (Just i) -> L $ fromIntegral i
        Nothing -> orig


-- | Extend the context covering the case where a subordinate letrec
-- is implemented by embedding the bindings directly into the parent's
-- bindings.
extendContextForEmbeddedScope :: (v -> Ref) -> SynVec -> (Var Int v -> Ref)
extendContextForEmbeddedScope context rs = context'
  where
    context' (B i) = rs `refIndex` i
    context' (F x) = context x
