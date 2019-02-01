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
import Eucalypt.Stg.IntrinsicInfo
import qualified Eucalypt.Stg.Intrinsics.Arithmetic as Arith
import qualified Eucalypt.Stg.Intrinsics.Block as Block
import qualified Eucalypt.Stg.Intrinsics.Dict as Dict
import qualified Eucalypt.Stg.Intrinsics.Emit as Emit
import qualified Eucalypt.Stg.Intrinsics.Meta as Meta
import qualified Eucalypt.Stg.Intrinsics.Number as Number
import qualified Eucalypt.Stg.Intrinsics.Panic as Panic
import qualified Eucalypt.Stg.Intrinsics.Set as Set
import qualified Eucalypt.Stg.Intrinsics.Str as Str
import qualified Eucalypt.Stg.Intrinsics.Time as Time
import qualified Eucalypt.Stg.Intrinsics.Eq as Eq
import qualified Eucalypt.Stg.Intrinsics.General as General
import Eucalypt.Stg.Machine

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "===" 2 Eq.natEq
  , IntrinsicInfo "EMIT{" 0 Emit.emitMappingStart
  , IntrinsicInfo "EMIT}" 0 Emit.emitMappingEnd
  , IntrinsicInfo "EMIT[" 0 Emit.emitSequenceStart
  , IntrinsicInfo "EMIT]" 0 Emit.emitSequenceEnd
  , IntrinsicInfo "EMITx" 1 Emit.emitScalar
  , IntrinsicInfo "EMIT0" 0 Emit.emitNull
  , IntrinsicInfo "META" 1 Meta.meta
  , IntrinsicInfo "WITHMETA" 2 Meta.withMeta
  , IntrinsicInfo "PANIC" 1 Panic.panic
  , IntrinsicInfo "PRUNE" 1 Block.prune
  , IntrinsicInfo "PRUNEMERGE" 2 Block.pruneMerge
  , IntrinsicInfo "CLOSED" 1 General.closed
  , IntrinsicInfo "NUMPARSE" 1 Number.parse
  ] <>
  Set.intrinsics <>
  Dict.intrinsics <>
  Str.intrinsics <>
  Arith.intrinsics <>
  Time.intrinsics

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
