{-|
Module      : Eucalypt.Stg.Intrinsics.Arithmetic
Description : Basic arithmetic built ins for the STG evaluator
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Arithmetic
  ( add
  , sub
  , mul
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Data.Vector ((!))

binop :: (Integer -> Integer -> Integer) -> MachineState -> ValVec -> IO MachineState
binop op ms (ValVec args) = do
  let (StgNat (NativeInt lhs)) = args ! 0
  let (StgNat (NativeInt rhs)) = args ! 1
  return $ tick $ setCode ms (ReturnLit (NativeInt (op lhs rhs)))

add :: MachineState -> ValVec -> IO MachineState
add = binop (+)

sub :: MachineState -> ValVec -> IO MachineState
sub = binop (-)

mul :: MachineState -> ValVec -> IO MachineState
mul = binop (*)
