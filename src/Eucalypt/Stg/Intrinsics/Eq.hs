{-|
Module      : Eucalypt.Stg.Intrinsics.Eq
Description : Intrinsics for equality
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Eq where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Data.Vector ((!))

natEq :: MachineState -> ValVec -> IO MachineState
natEq ms (ValVec xs) = do
  let (StgNat lhs) = xs ! 0
  let (StgNat rhs) = xs ! 1
  return $ setCode ms (ReturnLit (NativeBool (lhs == rhs)))
