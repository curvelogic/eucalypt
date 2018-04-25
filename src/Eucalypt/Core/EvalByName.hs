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


-- | Apply a builting once it is saturated, precondition is that
-- length of args == arity.
applyBuiltin ::
  WhnfEvaluator
  -> CoreExpr
  -> CoreBuiltinName
  -> [CoreExpr]
  -> Interpreter CoreExpr
applyBuiltin w expr name args = case lookupBuiltin name of
  Just (_, f) -> f w args
  Nothing -> throwEvalError $ BuiltinNotFound name expr


-- | Monadic WHNF to support abort and runtime error.
whnfM :: CoreExpr -> Interpreter CoreExpr

whnfM e@(CoreApp f x) = do
  f' <- whnfM f
  case f' of
    CoreLam body -> whnfM $ instantiate1 x body
    CorePAp arity expr args ->
      if length args < arity - 1
        then return (CorePAp arity expr (args ++ [x]))
        else (case expr of
                (CoreBuiltin name) -> applyBuiltin whnfM expr name args
                _ -> throwEvalError $ NotSupported "multiapply lambdas" e)
    expr -> throwEvalError $ UncallableExpression expr

whnfM (CoreLet bs b) = whnfM (inst b)
  where
    es = map inst bs
    inst = instantiate (es !!)

whnfM (CoreLookup e n) = euLookup whnfM e n

whnfM e@(CoreBuiltin n) = case lookupBuiltin n of
  Just (0, f) -> f whnfM []
  Just (arity, _) -> return (CorePAp arity e [])
  Nothing -> throwEvalError $ BuiltinNotFound n e

whnfM e@CorePAp{} = throwEvalError $ Bug "Found unevaluated saturated partial application." e

whnfM e = return e
