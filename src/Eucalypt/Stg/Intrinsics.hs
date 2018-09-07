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
import Data.Maybe (fromMaybe)
import qualified Eucalypt.Stg.Intrinsics.Arithmetic as Arith
import qualified Eucalypt.Stg.Intrinsics.Block as Block
import qualified Eucalypt.Stg.Intrinsics.Emit as Emit
import qualified Eucalypt.Stg.Intrinsics.Panic as Panic
import qualified Eucalypt.Stg.Intrinsics.Str as Str
import qualified Eucalypt.Stg.Intrinsics.Eq as Eq
import qualified Eucalypt.Stg.Intrinsics.General as General
import Eucalypt.Stg.Machine

data IntrinsicInfo = IntrinsicInfo
  { name :: String
  , arity :: Int
  , impl :: MachineState -> ValVec -> IO MachineState
  }

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "===" 2 Eq.natEq
  , IntrinsicInfo "ADD" 2 Arith.add
  , IntrinsicInfo "SUB" 2 Arith.sub
  , IntrinsicInfo "MUL" 2 Arith.mul
  , IntrinsicInfo "DIV" 2 Arith.divide
  , IntrinsicInfo "LT" 2 Arith.lt
  , IntrinsicInfo "GT" 2 Arith.gt
  , IntrinsicInfo "LTE" 2 Arith.lte
  , IntrinsicInfo "GTE" 2 Arith.gte
  , IntrinsicInfo "EMIT{" 0 Emit.emitMappingStart
  , IntrinsicInfo "EMIT}" 0 Emit.emitMappingEnd
  , IntrinsicInfo "EMIT[" 0 Emit.emitSequenceStart
  , IntrinsicInfo "EMIT]" 0 Emit.emitSequenceEnd
  , IntrinsicInfo "EMITx" 1 Emit.emitScalar
  , IntrinsicInfo "EMIT0" 0 Emit.emitNull
  , IntrinsicInfo "SPLIT" 2 Str.split
  , IntrinsicInfo "MATCH" 2 Str.match
  , IntrinsicInfo "MATCHES" 2 Str.matches
  , IntrinsicInfo "JOIN" 2 Str.join
  , IntrinsicInfo "PANIC" 1 Panic.panic
  , IntrinsicInfo "PRUNE" 1 Block.prune
  , IntrinsicInfo "PRUNEMERGE" 2 Block.pruneMerge
  , IntrinsicInfo "CLOSED" 1 General.closed
  , IntrinsicInfo "STRNAT" 1 Str.strNat
  , IntrinsicInfo "STRSYM" 1 Str.strSym
  ]

-- | Used during compilation to find the index at which an intrinsic
-- will be available
intrinsicIndex :: String -> Int
intrinsicIndex n =
  fromMaybe
    (error $ "No such intrinsic: " ++ n)
    (findIndex ((n ==) . name) intrinsics)

-- | Intrinsics packed into an array for faster access
intrinsicArray :: A.Array Int (MachineState -> ValVec -> IO MachineState)
intrinsicArray =
  let len = length intrinsics
   in A.array (0, len) [(i, impl (intrinsics !! i)) | i <- [0 .. len]]

-- | Used during runtime to access the intrinsic implementation
intrinsicFunction :: Int -> MachineState -> ValVec -> IO MachineState
intrinsicFunction i = intrinsicArray A.! i