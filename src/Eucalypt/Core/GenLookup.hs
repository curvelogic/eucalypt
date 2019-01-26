{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Core.GenLookup
Description : Tranform expression sequences to implement generalised lookup
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.GenLookup
  ( processGenLookup
  , eliminateLookupOp
  ) where

import Data.Char (isLetter)
import Eucalypt.Core.Syn

-- | During the transformation we may require different types of
-- lookup implementation.
--
-- Static generalised transformation can handle:
-- @
-- { a: 1 b: 2 }.[a, b]
-- @
--
-- Dynamic generalised transformation is needed for the general case:
-- @
-- f(x).[a, b]
-- @
--
-- Simple lookup is used when the rhs is a simple name (not a
-- general expression).
data LookupTransformation
  = SimpleLookup
  | StaticLookup
  | DynamicLookup
  | NoLookup
  deriving (Eq)



-- | State kept during this transformation, expressions in the op soup
-- are processed one by one and consed onto the output list.
--
-- The output list is reversed back to the proper order on completion.
data LookupTransformationState = LookupTransformationState
  { ltProcessedExprs :: [CoreExpr]
  , ltAction :: LookupTransformation
  , ltRemainingExprs :: [CoreExpr]
  }



-- | Initial state for the transformation
initState :: [CoreExpr] -> LookupTransformationState
initState = LookupTransformationState [] NoLookup



-- | Transformation is done when there are no expression remaining to
-- process
done :: LookupTransformationState -> Bool
done LookupTransformationState { ltRemainingExprs = [] } = True
done _ = False



-- | The output expressions from the transformation
result :: LookupTransformationState -> [CoreExpr]
result LookupTransformationState { .. } = reverse ltProcessedExprs



-- | A quick check that the variable name represents a normal variable
-- rather than an operator (which are excluded from generalised lookup)
isNonOperatorVar :: CoreBindingName -> Bool
isNonOperatorVar (c:_) = isLetter c || c == '•' || c == '$' || c == '?' || c == '_'
isNonOperatorVar _ = False


-- | Process expression(s) from the in list and pass to the out list
step :: LookupTransformationState -> LookupTransformationState

-- Request simple lookup transformation if we have an any other
-- expression prior to dot but simple name after the dot.
step s@LookupTransformationState { ltProcessedExprs = _
                                 , ltAction = NoLookup
                                 , ltRemainingExprs = CoreOperator _ InfixLeft _ (CoreBuiltin _ "*DOT*"):(e@(CoreName _ _)):es
                                 } =
  s {ltAction = SimpleLookup, ltRemainingExprs = e:es}

-- Request static gen lookup transformation if we have only a
-- DefaultBlockLet prior to dot and consume dot.
step s@LookupTransformationState { ltProcessedExprs = [CoreLet _ _ _ DefaultBlockLet]
                                 , ltAction = NoLookup
                                 , ltRemainingExprs = CoreOperator _ InfixLeft _ (CoreBuiltin _ "*DOT*"):es
                                 } =
  s {ltAction = StaticLookup, ltRemainingExprs = es}

-- Request dynamic gen lookup transformation if we have an any other
-- expression prior to dot and consume dot.
step s@LookupTransformationState { ltProcessedExprs = _
                                 , ltAction = NoLookup
                                 , ltRemainingExprs = CoreOperator _ InfixLeft _ (CoreBuiltin _ "*DOT*"):es
                                 } =
  s {ltAction = DynamicLookup, ltRemainingExprs = es}

-- If no lookup transformation required pass the expr right through
step s@LookupTransformationState { ltProcessedExprs = out
                                 , ltAction = NoLookup
                                 , ltRemainingExprs = e:es
                                 } =
  s {ltProcessedExprs = e : out, ltRemainingExprs = es}

-- Implement static gen lookup by @rebody@, works for simple too
step s@LookupTransformationState { ltProcessedExprs = [o@(CoreLet _ _ _ DefaultBlockLet)]
                                 , ltAction = StaticLookup
                                 , ltRemainingExprs = e:es
                                 } =
  let transformedExpr = rebody o (varify e)
   in s
        { ltProcessedExprs = [transformedExpr]
        , ltAction = NoLookup
        , ltRemainingExprs = es
        }

-- Implement simple lookup as rebody if we have a default block let
step s@LookupTransformationState { ltProcessedExprs = [o@(CoreLet _ _ _ DefaultBlockLet)]
                                 , ltAction = SimpleLookup
                                 , ltRemainingExprs = e@(CoreName _ _):es
                                 } =
  let transformedExpr = rebody o (varify e)
   in s
        { ltProcessedExprs = [transformedExpr]
        , ltAction = NoLookup
        , ltRemainingExprs = es
        }

-- Replace the dot in simple lookups, they'll be handled after
-- precedence (see 'eliminateLookupOp').
step s@LookupTransformationState { ltProcessedExprs = o:os
                                 , ltAction = SimpleLookup
                                 , ltRemainingExprs = e@(CoreName _ _):es
                                 } =
  s
    { ltProcessedExprs = e : lookupOp : o : os
    , ltAction = NoLookup
    , ltRemainingExprs = es
    }

-- Implement dynamic generalised lookup via transformation to lambda,
-- we require that all the names in the lookup expression that may
-- need to become lookups are already @CoreVar@s, not @CoreName@s.
--
-- Also replace the dot and finally organise for the application of
-- this lambda once all precedence is resolved (see
-- 'eliminateLookupOp').
step s@LookupTransformationState { ltProcessedExprs = os
                                 , ltAction = DynamicLookup
                                 , ltRemainingExprs = e:es
                                 } =
  s
    { ltProcessedExprs = dynamise e : lookupOp : os
    , ltAction = NoLookup
    , ltRemainingExprs = es
    }

step _ = error "Impossible step during generalised lookup transformation."

-- | When an expression appears after the '.', all free variables
-- (excluding operator names) inside it are potentially to be found
-- within the lookup object, not our wider binding environment so we
-- need to wrap the @CoreVar@ in a @CoreLookup@.
--
-- We leave the . lookup operator in place to allow precedence to be
-- resolved before we substitute the necessary cat operator to apply
-- this lambda.
--
dynamise :: CoreExpr -> CoreExpr
dynamise expr = lam (sourceMapId expr) [unq] $ expr >>= expand
  where
    unq = "___"
    expand v =
      if isNonOperatorVar v
        then CoreLookup 0 (CoreVar 0 unq) v (Just (CoreVar 0 v))
        else CoreVar 0 v



-- | Process any instances of generalised lookup
--
-- Works through soup identifying {..}.expr and turning expr into a
-- new body for the let representing the block. This has the effect of
-- also optimising @{ a: 1 }.a@ into @let a = 1 in a@ rather than a
-- lookup in a block.
processGenLookup :: [CoreExpr] -> [CoreExpr]
processGenLookup =
  result . head . dropWhile (not . done) . iterate step . initState



-- | Once precedences and fixities have been resolved, lookup and call
-- operators have become the functions in @CoreApply@ terms. We
-- transform these so that simple lookups (for which the argument is a
-- @CoreName@) become @CoreLookup@s and dynamic lookups (for which the
-- argument is a @CoreLambda@) become applications of the lambda.
eliminateLookupOp :: CoreExp a -> CoreExp a -> CoreExp a
eliminateLookupOp o (CoreName smid n) = CoreLookup smid o n Nothing
eliminateLookupOp o f@(CoreLambda smid _ _ _) = CoreApply smid f [o]
eliminateLookupOp _ _ =
  error "Lookup operator passed through cooking with unsupported argument."
