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


-- | The interpreter monad
newtype Interpreter a = Interpreter { runInterpreter :: Either CoreError a }
  deriving (Show, Functor, Applicative, Monad)


-- | Abort interpreter with 'CoreError'
throwEvalError :: CoreError -> Interpreter a
throwEvalError = Interpreter . Left



-- | A monadic evaluation function
type WhnfEvaluator = CoreExpr -> Interpreter CoreExpr
