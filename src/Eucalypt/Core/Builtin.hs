module Eucalypt.Core.Builtin where

import Eucalypt.Core.Syn
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Data.Either
import Data.List
import Debug.Trace



-- | Aggregate a list of interpreter results, concatenating any error
-- messages.
concatResults :: [Interpreter a] -> Interpreter [a]
concatResults results = let (errs, oks) = partitionEithers results in
  if null errs
  then
    Right oks
  else
    Left (RuntimeError $ concatMap errorMessage errs)




-- | Concatenate two lists or blocks into a list. The default action
-- for list concatenation and also available as `concat`.
euConcat :: WhnfEvaluator -> CoreExpr -> CoreExpr -> Interpreter CoreExpr
euConcat whnfM l r = do
  l' <- whnfM l
  r' <- whnfM r
  let items = (++) <$> extract l <*> extract r
  either (uncurry err) (Right . CoreList) items

  where extract e = case e of
          CoreList items -> Right items
          CoreBlock (CoreList items) -> Right items
          _ -> Left ("Concat argument not a list", e)




-- | Block merge. The default action for block catentation (or
-- applying a block as a function). Also available as `merge`.
euMerge :: WhnfEvaluator -> CoreExpr -> CoreExpr -> Interpreter CoreExpr
euMerge whnfM l r = CoreBlock <$> euConcat whnfM l r




-- | Lookup in a block requires realising all keys as later keys
-- override earlier. Missing key is a RuntimeError.
--
-- TODO: should we allow lookup of integers in lists here? not yet
euLookup :: WhnfEvaluator -> CoreExpr -> CoreRelativeName -> Interpreter CoreExpr
euLookup whnfM e name = do
  obj <- whnfM e

  case obj of

    CoreBlock (CoreList items) ->

                           do
                             alist <- buildSearchList items

                             case lookup (CorePrim (Symbol name)) alist of
                               Just val -> return val
                               Nothing -> runtimeError ("Key " ++ name ++ " not found")

                           where buildSearchList items = concatResults (map eval (reverse items))

                                 eval item = do
                                   item' <- whnfM item
                                   case item' of
                                     CoreList [k, v] -> whnfM k >>= \k -> return (k, v)
                                     i@_ -> err "Bad key in block element " i

    _ -> err "Lookup in non-block" e
