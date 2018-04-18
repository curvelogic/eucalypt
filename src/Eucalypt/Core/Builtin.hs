{-|
Module      : Eucalypt.Core.Builtin
Description : Implementations of built in functions
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Builtin
  ( euConcat, euMerge, euLookup )
where

import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn



-- | Concatenate two lists or blocks into a list. The default action
-- for list concatenation and also available as `concat`.
euConcat :: WhnfEvaluator -> CoreExpr -> CoreExpr -> Interpreter CoreExpr
euConcat whnfM l r = do
  l' <- whnfM l
  r' <- whnfM r
  items <- (++) <$> extract l' <*> extract r'
  return $ CoreList items
  where
    extract :: CoreExpr -> Interpreter [CoreExpr]
    extract e =
      case e of
        CoreList items -> return items
        CoreBlock (CoreList items) -> return items
        _ -> throwEvalError $ ConcatArgumentNotList e



-- | Block merge. The default action for block catentation (or
-- applying a block as a function). Also available as `merge`.
euMerge :: WhnfEvaluator -> CoreExpr -> CoreExpr -> Interpreter CoreExpr
euMerge whnfM l r = CoreBlock <$> euConcat whnfM l r



-- | Unwrap a single block element into name and expr, evaluating the
-- key if necessary to get there.
unwrapBlockElement :: WhnfEvaluator -> CoreExpr -> Interpreter (String, CoreExpr)
unwrapBlockElement whnfM expr@(CoreList [k, v]) = do
  key <- whnfM k
  case key of
    CorePrim (Symbol n) -> return (n, v)
    _ -> throwEvalError $ BadBlockElement expr
unwrapBlockElement _ expr = throwEvalError $ BadBlockElement expr



-- | From a block, expose an association list for lookup
buildSearchList :: WhnfEvaluator -> CoreExpr -> Interpreter [(String, CoreExpr)]
buildSearchList whnfM (CoreBlock (CoreList items)) =
  concatResults $ map eval (reverse items)
  where
    eval item = whnfM item >>= unwrapBlockElement whnfM
buildSearchList _ e = throwEvalError $ LookupTargetNotList e



-- | Lookup in a block requires realising all keys as later keys
-- override earlier. Missing key is a EvaluationError.
--
-- TODO: should we allow lookup of integers in lists here? not yet
euLookup :: WhnfEvaluator -> CoreExpr -> CoreRelativeName -> Interpreter CoreExpr
euLookup whnfM e name = do
  obj <- whnfM e
  alist <- buildSearchList whnfM obj
  case lookup name alist of
    Just val -> return val
    Nothing -> throwEvalError $ KeyNotFound name
