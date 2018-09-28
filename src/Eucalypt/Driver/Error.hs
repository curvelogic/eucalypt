{-|
Module      : Eucalypt.Core.Error
Description : Errors detected during driver stages
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Driver.Error where

import Control.Exception.Safe
import Eucalypt.Syntax.Input (Input)

data CommandError
  = InvalidInput Input -- ^ invalid input (bad format etc.)
  | CyclicInputs [Input] -- ^ input imports form one or more cycles
  deriving (Show, Typeable)

instance Exception CommandError
