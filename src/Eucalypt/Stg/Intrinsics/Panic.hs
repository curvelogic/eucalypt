{-|
Module      : Eucalypt.Stg.Intrinsics.Panic
Description : Various error builtins in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Panic
  ( intrinsics
  ) where

import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Error
import Eucalypt.Stg.Machine

intrinsics :: [IntrinsicInfo]
intrinsics = [IntrinsicInfo "PANIC" 1 (invoke panic)]

panic :: MachineState -> String -> IO MachineState
panic ms s = throwIn ms $ Panic s
