{-|
Module      : Eucalypt.Stg.Intrinsics.IOHMBlock
Description : Block implementation using stored insert ordered hash map
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.IOHMBlock where

import Control.Monad (liftM2, join, foldM)
import Data.Dynamic
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Strict.InsOrd as OM
import Eucalypt.Stg.Error
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Vec

-- | Insert ordered hash map of string (symbol) to STG value
type IOHM = OM.InsOrdHashMap String StgValue

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "IOHM.EMPTY" 0 (invoke emptyIohm)
  , IntrinsicInfo "IOHM.INSERT" 3 (invoke iohmInsert)
  , IntrinsicInfo "IOHM.LIST" 1 (invoke iohmList)
  , IntrinsicInfo "IOHM.LOOKUP" 2 (invoke iohmLookup)
  , IntrinsicInfo "IOHM.LOOKUPOR" 3 (invoke iohmLookupOr)
  , IntrinsicInfo "IOHM.MERGE" 2 (invoke iohmMerge)
  , IntrinsicInfo "IOHM.MERGEWITH" 3 (invoke iohmMergeWith)
  ]

-- | Return an empty IOHM
emptyIohm :: MachineState -> IO MachineState
emptyIohm ms = returnDynamic ms (OM.empty :: IOHM)

-- | Add a single kv into the interm IOHM
iohmInsert :: MachineState -> Dynamic -> Symbol -> StgValue -> IO MachineState
iohmInsert ms dyn (Symbol k) v =
  cast ms dyn >>= returnDynamic ms . OM.insertWith const k v

-- | Return contest as kv list
iohmList :: MachineState -> Dynamic -> IO MachineState
iohmList ms dyn = do
  om <- cast ms dyn :: IO IOHM
  let nilAddr = retrieveGlobal ms "KNIL"
  pairAddrs <- traverse (allocPair nilAddr) $ OM.toList om
  case pairAddrs of
    [] -> returnNil ms
    (h:t) -> do
      tv <- foldM flipCons nilAddr (reverse t)
      return $ ms {machineCode = ReturnCon stgCons (toVec [h, tv]) Nothing}
  where
    allocPair nil (k, v) =
      foldM flipCons nil [v, StgNat (NativeSymbol k) Nothing]

-- | Lookup
iohmLookup :: MachineState -> Dynamic -> Symbol -> IO MachineState
iohmLookup ms dyn (Symbol k) = do
  om <- cast ms dyn :: IO IOHM
  case OM.lookup k om of
    (Just v) -> returnValue ms v
    Nothing -> throwIn ms $ KeyNotFound (NativeSymbol k)

-- | LookupOr
iohmLookupOr :: MachineState -> Dynamic -> Symbol -> StgValue -> IO MachineState
iohmLookupOr ms dyn (Symbol k) dft = do
  om <- cast ms dyn :: IO IOHM
  returnValue ms $ fromMaybe dft (OM.lookup k om)

-- | Shallow merge two IOHMs
iohmMerge :: MachineState -> Dynamic -> Dynamic -> IO MachineState
iohmMerge ms l r = do
  xs <- cast ms l :: IO IOHM
  ys <- cast ms r :: IO IOHM
  returnDynamic ms $ OM.unionWith (flip const) xs ys

-- | A monadic version of OM.unionWith
unionWithM :: IOHM -> IOHM -> (StgValue -> StgValue -> IO StgValue) -> IO IOHM
unionWithM l r f =
  let lM = OM.map return l
      rM = OM.map return r
   in sequence $ OM.unionWith fM lM rM
  where
    fM x y = join $ liftM2 f x y

-- | Merge using combining function for duplicates
iohmMergeWith :: MachineState -> Dynamic -> Dynamic -> Address -> IO MachineState
iohmMergeWith ms l r f = do
  xs <- cast ms l :: IO IOHM
  ys <- cast ms r :: IO IOHM
  merged <- unionWithM xs ys combined
  returnDynamic ms merged
  where
    combined :: StgValue -> StgValue -> IO StgValue
    combined x y = StgAddr <$> allocFXs ms f (toVec [x, y])
