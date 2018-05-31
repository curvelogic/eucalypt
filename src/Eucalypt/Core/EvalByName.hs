{-# LANGUAGE LambdaCase #-}
{-|
Module      : Eucalypt.Core.EvalByName
Description : A crude call-by-name substitutional evaluator to get going with...
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.EvalByName where

import Control.Monad ((>=>))
import Debug.Trace
import Eucalypt.Core.Builtin
import Eucalypt.Core.Cook
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
applyBuiltin w expr name as =
  case lookupBuiltin name of
    Just (_, f) -> f w as >>= w
    Nothing -> throwEvalError $ BuiltinNotFound name expr

-- $ metadata
--
-- The evaluator respects the following metadata controls:
--
-- - "trace" (boolean)
-- - "assert" (function / pap) to be passed value

-- | Peel trace metadata out of 'CoreMeta' and into 'CoreTraced'
separateTraceMeta :: WhnfEvaluator -> CoreExpr -> Interpreter CoreExpr
separateTraceMeta w e@(CoreMeta m v) =
  lookupMaybe w m "trace" >>= \case
    Just (CorePrim (CoreBoolean b)) ->
      stripTrace m >>= \meta -> return $ applyMeta meta $ traceIf b v
    _ -> return e
  where
    stripTrace meta = removeItem w meta "trace"
    applyMeta meta value =
      if meta == block []
        then value
        else CoreMeta meta v
    traceIf b value =
      if b
        then CoreTraced value
        else value
separateTraceMeta _ e = throwEvalError $ Bug "Bad call to separateTraceMeta" e

-- | Peel assert metadata out of 'CoreMeta' and into 'CoreTraced'
separateAssertMeta :: WhnfEvaluator -> CoreExpr -> Interpreter CoreExpr
separateAssertMeta w e@(CoreMeta m v) =
  lookupMaybe w m "assert" >>= \case
    Just fn ->
      stripAssert m >>= \meta -> return $ applyMeta meta (CoreChecked fn v)
    Nothing -> return e
  where
    stripAssert meta = removeItem w meta "assert"
    applyMeta meta value =
      if meta == block []
        then value
        else CoreMeta meta value
separateAssertMeta _ e = throwEvalError $ Bug "Bad call to separateAssertMeta" e



-- | New style multi-arg application
--
--
handleApply :: CoreExpr -> [CoreExpr] -> Interpreter CoreExpr
handleApply expr [] = return expr
handleApply (CorePAp arity expr as) newArgs =
  let args' = (as ++ newArgs)
   in if length args' < arity
        then return (CorePAp arity expr args')
        else (case expr of
                (CoreBuiltin name) -> applyBuiltin whnfM expr name args'
                f -> handleApply f args')
handleApply (CoreMeta _m f) as = whnfM f >>= (`handleApply` as)
handleApply (f@CoreBlock {}) (x:xs) =
  whnfM x >>= \b ->
    case b of
      CoreBlock {} -> euMerge whnfM [b, f] >>= (`handleApply` xs)
      _ -> throwEvalError $ BadBlockMerge b
handleApply f@(CoreLambda n b) as
  | length as > n = whnfM (instantiateBody (take n as) b) >>= (`handleApply` drop n as)
  | length as == n = whnfM $ instantiateBody as b
  | length as < n = return (CorePAp n f as)
handleApply (CoreOperator _ _ expr) as = whnfM expr >>= (`handleApply` as)
handleApply expr _ = throwEvalError $ UncallableExpression expr


-- | Monadic WHNF to support abort and runtime error.
--
whnfM :: CoreExpr -> Interpreter CoreExpr
whnfM (CoreApply f as) = whnfM f >>= (`handleApply` as)
whnfM e@CoreLet {} = whnfM $ instantiateLet e
whnfM (CoreOpSoup exprs) = cook exprs >>= whnfM
whnfM (CoreLookup e n) = lookupName whnfM e n >>= whnfM
whnfM e@(CoreBuiltin n) =
  case lookupBuiltin n of
    Just (0, f) -> f whnfM []
    Just (arity, _) -> return (CorePAp arity e [])
    Nothing -> throwEvalError $ BuiltinNotFound n e
whnfM (CoreMeta m e) = do
  metadata <- whnfM m -- should be in isolated metadata binding env
  separated <- (separateTraceMeta whnfM >=> separateAssertMeta whnfM) (CoreMeta metadata e)
  case separated of
    (CoreMeta m' v) -> CoreMeta m' <$> whnfM v
    expr -> whnfM expr
whnfM (CoreTraced e) = trace ("TRACE: " ++ show e) whnfM e
whnfM (CoreChecked fn e) =
  whnfM (CoreApply fn [e]) >>= \case
    (CorePrim (CoreBoolean True)) -> whnfM e
    _ -> throwEvalError $ AssertionFailed e
whnfM e = return e
