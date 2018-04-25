{-|
Module      : Eucalypt.Core.EvalByName
Description : A simple call-by-name evaluator to get going with...
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.EvalByName
  where

import Bound
import Eucalypt.Core.Syn
import Eucalypt.Core.Error
import Eucalypt.Core.Builtin
import Eucalypt.Core.Interpreter


-- | Monadic WHNF to support abort and runtime error.
whnfM :: CoreExpr -> Interpreter CoreExpr

whnfM (CoreApp f x) = do
  f' <- whnfM f
  case f' of
    CoreLam e -> whnfM $ instantiate1 x e
    e@_ -> throwEvalError $ UncallableExpression e

whnfM (CoreLet bs b) = whnfM (inst b)
  where
    es = map inst bs
    inst = instantiate (es !!)

whnfM (CoreLookup e n) = euLookup whnfM e n

whnfM (CoreBuiltin _) = undefined

whnfM e = return e
