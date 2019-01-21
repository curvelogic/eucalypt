{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
  , divide
  , lt
  , gt
  , lte
  , gte
  , modulo
  , flr
  , ceil
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Data.Fixed (mod')
import Data.Scientific
import Data.Sequence ((!?))

binop ::
     (Scientific -> Scientific -> Scientific)
  -> MachineState
  -> ValVec
  -> IO MachineState
binop op ms (ValVec args) = do
  let (Just (StgNat (NativeNumber lhs) _)) = args !? 0
  let (Just (StgNat (NativeNumber rhs) _)) = args !? 1
  return $ setCode ms (ReturnLit (NativeNumber (op lhs rhs)) Nothing)

add :: MachineState -> ValVec -> IO MachineState
add = binop (+)

sub :: MachineState -> ValVec -> IO MachineState
sub = binop (-)

mul :: MachineState -> ValVec -> IO MachineState
mul = binop (*)

modulo :: MachineState -> ValVec -> IO MachineState
modulo = binop mod'

sciDivide :: Scientific -> Scientific -> Scientific
sciDivide l r =
  let lr = toRational l
      rr = toRational r
      result = lr / rr
      float = fromRational result :: Double
   in fromFloatDigits float

divide :: MachineState -> ValVec -> IO MachineState
divide = binop sciDivide

binopBool ::
     (Scientific -> Scientific -> Bool)
  -> MachineState
  -> ValVec
  -> IO MachineState
binopBool op ms (ValVec args) = do
  let (Just (StgNat (NativeNumber lhs) _)) = args !? 0
  let (Just (StgNat (NativeNumber rhs) _)) = args !? 1
  return $ setCode ms (ReturnLit (NativeBool (op lhs rhs)) Nothing)

lt :: MachineState -> ValVec -> IO MachineState
lt = binopBool (<)

gt :: MachineState -> ValVec -> IO MachineState
gt = binopBool (>)

lte :: MachineState -> ValVec -> IO MachineState
lte = binopBool (<=)

gte :: MachineState -> ValVec -> IO MachineState
gte = binopBool (>=)

unop ::
     (Scientific -> Scientific)
  -> MachineState
  -> ValVec
  -> IO MachineState
unop op ms (ValVec args) = do
  let (Just (StgNat (NativeNumber n) _)) = args !? 0
  return $ setCode ms (ReturnLit (NativeNumber (op n)) Nothing)

flr :: MachineState -> ValVec -> IO MachineState
flr = unop (fromIntegral . floor)

ceil :: MachineState -> ValVec -> IO MachineState
ceil = unop (fromIntegral . ceiling)
