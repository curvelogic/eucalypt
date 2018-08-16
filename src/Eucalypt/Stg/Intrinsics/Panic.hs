{-|
Module      : Eucalypt.Stg.Intrinsics.Panic
Description : Various error builtins in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Panic where

import Control.Exception.Safe
import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Data.Vector ((!))

panic :: MachineState -> ValVec -> IO MachineState
panic _ (ValVec xs) = do
  let (StgNat (NativeString s)) = xs ! 0
  throwM $ Panic s
