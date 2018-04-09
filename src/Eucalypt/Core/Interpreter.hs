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


-- | The interpreter monad
type Interpreter a = Either (RuntimeError String) a



-- | A monadic evaluation function
type WhnfEvaluator = CoreExpr -> Interpreter CoreExpr



-- | Throw error
err :: String -> CoreExpr -> Interpreter b
err s e = (Left . RuntimeError) (s ++ ": " ++ presentExpressionInError e)
