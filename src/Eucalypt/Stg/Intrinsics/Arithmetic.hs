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
  ( intrinsics
  ) where

import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common (IntrinsicFunction, invoke)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Tags
import Data.Fixed (mod')
import Data.Scientific

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "ADD" 2 add
  , IntrinsicInfo "SUB" 2 sub
  , IntrinsicInfo "MUL" 2 mul
  , IntrinsicInfo "DIV" 2 divide
  , IntrinsicInfo "LT" 2 lt
  , IntrinsicInfo "GT" 2 gt
  , IntrinsicInfo "LTE" 2 lte
  , IntrinsicInfo "GTE" 2 gte
  , IntrinsicInfo "MOD" 2 modulo
  , IntrinsicInfo "FLOOR" 1 flr
  , IntrinsicInfo "CEILING" 1 ceil
  ]

binop :: (Scientific -> Scientific -> Scientific) -> IntrinsicFunction
binop op =
  invoke
    (\ms lhs rhs ->
       (return $ setCode ms (ReturnLit (NativeNumber (op lhs rhs)) Nothing)) :: IO MachineState)

add :: IntrinsicFunction
add = binop (+)

sub :: IntrinsicFunction
sub = binop (-)

mul :: IntrinsicFunction
mul = binop (*)

modulo :: IntrinsicFunction
modulo = binop mod'

sciDivide :: Scientific -> Scientific -> Scientific
sciDivide l r =
  let lr = toRational l
      rr = toRational r
      result = lr / rr
      float = fromRational result :: Double
   in fromFloatDigits float

divide :: IntrinsicFunction
divide = binop sciDivide

binopBool :: (Scientific -> Scientific -> Bool) -> IntrinsicFunction
binopBool op =
  invoke
    (\ms lhs rhs ->
       (return $ setCode ms (ReturnCon (boolTag (op lhs rhs)) mempty Nothing)) :: IO MachineState)

lt :: IntrinsicFunction
lt = binopBool (<)

gt :: IntrinsicFunction
gt = binopBool (>)

lte :: IntrinsicFunction
lte = binopBool (<=)

gte :: IntrinsicFunction
gte = binopBool (>=)

unop ::
     (Scientific -> Scientific)
  -> IntrinsicFunction
unop op =
  invoke
    (\ms n ->
       (return $ setCode ms (ReturnLit (NativeNumber (op n)) Nothing)) :: IO MachineState)

flr :: IntrinsicFunction
flr = unop (fromIntegral . floor)

ceil :: IntrinsicFunction
ceil = unop (fromIntegral . ceiling)
