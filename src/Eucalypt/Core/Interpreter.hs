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
type Interpreter a = Either EvaluationError a



-- | A monadic evaluation function
type WhnfEvaluator = CoreExpr -> Interpreter CoreExpr



-- | Aggregate a list of interpreter results, concatenating any error
-- messages.
concatResults :: [Interpreter a] -> Interpreter [a]
concatResults results = let (errs, oks) = partitionEithers results in
  if null errs
  then
    Right oks
  else
    Left (MultipleErrors errs)
