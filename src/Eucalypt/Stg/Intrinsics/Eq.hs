{-|
Module      : Eucalypt.Stg.Intrinsics.Eq
Description : Intrinsics for equality
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Eq
  ( intrinsics
  ) where

import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native

intrinsics :: [IntrinsicInfo]
intrinsics = [IntrinsicInfo "===" 2 (invoke natEq)]

natEq :: MachineState -> Native -> Native -> IO MachineState
natEq ms l r =  returnBool ms $ l == r
