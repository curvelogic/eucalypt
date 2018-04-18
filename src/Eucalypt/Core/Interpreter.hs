{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Eucalypt.Core.Interpreter
Description : The Intepreter monad and related types
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Interpreter
  where

import Eucalypt.Core.Error
import Eucalypt.Core.Syn
import Data.Either



-- | The interpreter monad
newtype Interpreter a = Interpreter { runInterpreter :: Either EvaluationError a }
  deriving (Show, Eq, Functor, Applicative, Monad)



-- | Abort interpreter with 'EvaluationError'
throwEvalError :: EvaluationError -> Interpreter a
throwEvalError = Interpreter . Left



-- | A monadic evaluation function
type WhnfEvaluator = CoreExpr -> Interpreter CoreExpr



-- | Aggregate a list of interpreter results, concatenating any error
-- messages.
concatResults :: [Interpreter a] -> Interpreter [a]
concatResults results = let (errs, oks) = partitionEithers (map runInterpreter results) in
  if null errs
  then
    return oks
  else
    throwEvalError $ MultipleErrors errs
