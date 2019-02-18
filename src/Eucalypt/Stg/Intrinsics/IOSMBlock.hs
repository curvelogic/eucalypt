{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Eucalypt.Stg.Intrinsics.IOSMBlock
Description : Block implementation using stored insert ordered hash map
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.IOSMBlock where

import Data.Dynamic
import Data.Maybe (fromMaybe)
import Data.Symbol
import Eucalypt.Stg.Error
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Intrinsics.SymbolMap as SM
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Stg.Vec

-- | Insert ordered hash map of string (symbol) to STG value
type IOSM = SM.InsOrdSymbolMap StgValue


intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "IOSM.EMPTY" 0 (invoke emptyIosm)
  , IntrinsicInfo "IOSM.INSERT" 3 (invoke iosmInsert)
  , IntrinsicInfo "IOSM.LIST" 1 (invoke iosmList)
  , IntrinsicInfo "IOSM.FROMALIST" 1 (invoke iosmFromAList)
  , IntrinsicInfo "IOSM.LOOKUP" 2 (invoke iosmLookup)
  , IntrinsicInfo "IOSM.LOOKUPOR" 3 (invoke iosmLookupOr)
  , IntrinsicInfo "IOSM.MERGE" 2 (invoke iosmMerge)
  , IntrinsicInfo "IOSM.MERGEWITH" 3 (invoke iosmMergeWith)
  ]

-- | Return an empty IOSM
emptyIosm :: MachineState -> IO MachineState
emptyIosm ms = returnDynamic ms (SM.empty :: IOSM)

-- | Add a single kv into the interm IOSM
iosmInsert :: MachineState -> Dynamic -> Symbol -> StgValue -> IO MachineState
iosmInsert ms dyn k v =
  cast ms dyn >>= returnDynamic ms . SM.insertWith const k v

-- | Return contest as kv list
iosmList :: MachineState -> Dynamic -> IO MachineState
iosmList ms dyn = do
  om <- cast ms dyn :: IO IOSM
  returnList ms $ SM.toList om

-- | From an association list (forced with seqPairList or similar),
-- return an unwrapped IOSM
iosmFromAList :: MachineState -> Address -> IO MachineState
iosmFromAList ms a = do
  kvs <- scrape ms (StgAddr a) :: IO (Maybe [(Symbol, StgValue)])
  let om = maybe SM.empty SM.fromList kvs
  returnDynamic ms om

-- | Lookup
iosmLookup :: MachineState -> Dynamic -> Symbol -> IO MachineState
iosmLookup ms dyn k = do
  om <- cast ms dyn :: IO IOSM
  case SM.lookup k om of
    (Just v) -> returnValue ms v
    Nothing -> throwIn ms $ KeyNotFound (NativeSymbol k)

-- | LookupOr
iosmLookupOr :: MachineState -> Dynamic -> Symbol -> StgValue -> IO MachineState
iosmLookupOr ms dyn k dft = do
  om <- cast ms dyn :: IO IOSM
  returnValue ms $ fromMaybe dft (SM.lookup k om)

-- | Shallow merge two IOSMs
iosmMerge :: MachineState -> Dynamic -> Dynamic -> IO MachineState
iosmMerge ms l r = do
  xs <- cast ms l :: IO IOSM
  ys <- cast ms r :: IO IOSM
  returnDynamic ms $ SM.unionWith (flip const) xs ys

-- | Merge using combining function for duplicates
iosmMergeWith :: MachineState -> Dynamic -> Dynamic -> Address -> IO MachineState
iosmMergeWith ms l r f = do
  xs <- cast ms l :: IO IOSM
  ys <- cast ms r :: IO IOSM
  merged <- unionWithM xs ys combined
  returnDynamic ms merged
  where
    combined :: StgValue -> StgValue -> IO StgValue
    combined x y = StgAddr <$> allocFXs ms f (toVec [x, y])
