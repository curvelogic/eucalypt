{-|
Module      : Eucalypt.Core.Error
Description : RuntimeError implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Error
  where

import Eucalypt.Core.Syn
import Eucalypt.Core.Pretty

data RuntimeError e = RuntimeError { errorMessage :: e }
  deriving (Show, Eq)

type Result e a = Either (RuntimeError e) a

runtimeError :: e -> Result e a
runtimeError s = Left $ RuntimeError s

presentExpressionInError :: CoreExp CoreBindingName -> String
presentExpressionInError exp = pprint exp
