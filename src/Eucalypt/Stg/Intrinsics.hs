{-|
Module      : Eucalypt.Stg.Intrinsics
Description : Table of intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics where

import qualified Data.Array as A
import Data.List (findIndex)
import qualified Eucalypt.Stg.Intrinsics.Arithmetic as Arith
import Eucalypt.Stg.Machine

data IntrinsicInfo = IntrinsicInfo
  { name :: String
  , arity :: Int
  , impl :: MachineState -> ValVec -> IO MachineState
  }

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "ADD" 2 Arith.add
  , IntrinsicInfo "SUB" 2 Arith.sub
  , IntrinsicInfo "MUL" 2 Arith.mul
  ]

-- | Used during compilation to find the index at which an intrinsic
-- will be available
intrinsicIndex :: String -> Maybe Int
intrinsicIndex n = findIndex ((n ==) . name) intrinsics

-- | Intrinsics packed into an array for faster access
intrinsicArray :: A.Array Int (MachineState -> ValVec -> IO MachineState)
intrinsicArray =
  let len = length intrinsics
   in A.array (0, len) [(i, impl (intrinsics !! i)) | i <- [0 .. len]]

-- | Used during runtime to access the intrinsic implementation
intrinsicFunction :: Int -> MachineState -> ValVec -> IO MachineState
intrinsicFunction i = intrinsicArray A.! i
