{-|
Module      : Eucalypt.Stg.IntrinsicInfo
Description : Description of intrinsic function
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.IntrinsicInfo where

import Eucalypt.Stg.Value
import Eucalypt.Stg.Machine

data IntrinsicInfo = IntrinsicInfo
  { name :: String
  , arity :: Int
  , impl :: MachineState -> ValVec -> IO MachineState
  }
