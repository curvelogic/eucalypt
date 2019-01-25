{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Core.GenLookup
Description : Tranform expression sequences to implement generalised lookup
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.GenLookup where

import Eucalypt.Core.Syn

-- | During the transformation we may require different types of
-- generalised lookup implementation.
--
-- Static transformation can handle:
-- @
-- { a: 1 b: 2 }.[a, b]
-- @
--
-- Dynamic transformation is needed for the general case:
-- @
-- f(x).[a, b]
-- @
data LookupTransformation = StaticLookup | DynamicLookup | NoLookup
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



-- | Process expression(s) from the in list and pass to the out list
step :: LookupTransformationState -> LookupTransformationState

-- Request static gen lookup transformation if we have a
-- DefaultBlockLet prior to dot and consume dot.
step s@LookupTransformationState { ltProcessedExprs = CoreLet _ _ _ DefaultBlockLet:_
                                 , ltAction = NoLookup
                                 , ltRemainingExprs = CoreOperator _ InfixLeft _ (CoreBuiltin _ "*DOT*"):es
                                 } =
  s {ltAction = StaticLookup, ltRemainingExprs = es}

-- Request dynamic gen lookup transformation if we have an OtherLet
-- prior to dot and consume dot.
step s@LookupTransformationState { ltProcessedExprs = CoreLet _ _ _ OtherLet:_
                                 , ltAction = NoLookup
                                 , ltRemainingExprs = CoreOperator _ InfixLeft _ (CoreBuiltin _ "*DOT*"):es
                                 } =
  s {ltAction = StaticLookup, ltRemainingExprs = es}

-- If no lookup transformation required pass the expr right through
step s@LookupTransformationState { ltProcessedExprs = out
                                 , ltAction = NoLookup
                                 , ltRemainingExprs = e:es
                                 } =
  s {ltProcessedExprs = e : out, ltRemainingExprs = es}

-- Implement static gen lookup by @rebody@
step s@LookupTransformationState { ltProcessedExprs = o@(CoreLet _ _ _ DefaultBlockLet):os
                                 , ltAction = StaticLookup
                                 , ltRemainingExprs = e:es
                                 } =
  let transformedExpr = rebody o (varify e)
   in s
        { ltProcessedExprs = transformedExpr : os
        , ltAction = NoLookup
        , ltRemainingExprs = es
        }

-- Implement dynamic lookup via transformation to lambda
step s@LookupTransformationState { ltProcessedExprs = o@(CoreLet _ _ _ OtherLet):os
                                 , ltAction = DynamicLookup
                                 , ltRemainingExprs = e:es
                                 } =
  s
    { ltProcessedExprs = dynamise e : catOp : o : os
    , ltAction = NoLookup
    , ltRemainingExprs = es
    }

step _ = error "Impossible step during generalised lookup transformation."

-- | When an expression appears after the '.', all free variables
-- inside it are potentially to be found within the lookup object,
-- not our wider binding environment so we need to wrap the @CoreVar@
-- in a @CoreLookup@.
--
dynamise :: CoreExpr -> CoreExpr
dynamise expr =
  let unq = "___"
   in lam (sourceMapId expr) [unq] $
      expr >>= (\v -> CoreLookup 0 (CoreVar 0 unq) v (Just (CoreVar 0 v)))



-- | Process any instances of generalised lookup
--
-- Works through soup identifying {..}.expr and turning expr into a
-- new body for the let representing the block. This has the effect of
-- also optimising @{ a: 1 }.a@ into @let a = 1 in a@ rather than a
-- lookup in a block.
processGenLookup :: [CoreExpr] -> [CoreExpr]
processGenLookup =
  result . head . dropWhile (not . done) . iterate step . initState
