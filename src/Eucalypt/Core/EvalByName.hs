{-|
Module      : Eucalypt.Core.EvalByName
Description : A crude call-by-name substitutional evaluator to get going with...
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.EvalByName where

import Eucalypt.Core.Builtin
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn

-- | Apply a builting once it is saturated, precondition is that
-- length of args == arity. Reduce return to WHNF. (Builtins
-- themselves are under no obligaion to do so.)
applyBuiltin ::
     WhnfEvaluator
  -> CoreExpr
  -> CoreBuiltinName
  -> [CoreExpr]
  -> Interpreter CoreExpr
applyBuiltin w expr name args =
  case lookupBuiltin name of
    Just (_, f) -> f w args >>= w
    Nothing -> throwEvalError $ BuiltinNotFound name expr


-- | Monadic WHNF to support abort and runtime error.
--
whnfM :: CoreExpr -> Interpreter CoreExpr
whnfM e@(CoreApp f x) = do
  f' <- whnfM f
  case f' of
    CoreBlock{} -> whnfM x >>= \a -> case a of
                     CoreBlock{} -> euMerge whnfM [a, f]
                     _ -> throwEvalError $ BadBlockMerge e
    l@CoreLam{} -> whnfM $ instantiateLambda x l
    CorePAp arity expr args ->
      let args' = (args ++ [x])
       in if length args' < arity
            then return (CorePAp arity expr args')
            else (case expr of
                    (CoreBuiltin name) -> applyBuiltin whnfM expr name args'
                    _ -> throwEvalError $ NotSupported "multiapply lambdas" e)
    expr -> throwEvalError $ UncallableExpression expr
whnfM e@CoreLet {} = whnfM $ instantiateLet e
whnfM (CoreLookup e n) = lookupName whnfM e n -- TODO: generalised lookup
whnfM e@(CoreBuiltin n) =
  case lookupBuiltin n of
    Just (0, f) -> f whnfM []
    Just (arity, _) -> return (CorePAp arity e [])
    Nothing -> throwEvalError $ BuiltinNotFound n e
whnfM e@CorePAp {} =
  throwEvalError $ Bug "Found unevaluated saturated partial application." e
whnfM e = return e
