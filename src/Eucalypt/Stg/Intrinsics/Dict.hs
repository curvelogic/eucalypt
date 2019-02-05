{-|
Module      : Eucalypt.Stg.Intrinsics.Dict
Description : Basic dict built-ins for the STG evaluator
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Dict
  ( intrinsics
  ) where

import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Error
import Eucalypt.Stg.Native
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Tags
import qualified Data.Map.Strict as MS

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "EMPTYDICT" 0 (invoke emptyDict)
  , IntrinsicInfo "DICTCONTAINSKEY" 2 (invoke dictContainsKey)
  , IntrinsicInfo "DICTGET" 2 (invoke dictGet)
  , IntrinsicInfo "DICTPUT" 2 (invoke dictPut)
  , IntrinsicInfo "DICTDEL" 2 (invoke dictDel)
  , IntrinsicInfo "DICTENTRIES" 2 (invoke dictEntries)
  ]

-- | __EMPTYDICT
emptyDict :: MachineState -> IO MachineState
emptyDict ms = return $ setCode ms (ReturnLit (NativeDict MS.empty) Nothing)

-- | __DICTCONTAINSKEY(d, k)
dictContainsKey :: MachineState -> MS.Map Native Native -> Native -> IO MachineState
dictContainsKey ms d k =
  return $ setCode ms (ReturnCon (boolTag $ k `MS.member` d) mempty Nothing)

-- | __DICTGET(d, k)
dictGet :: MachineState -> MS.Map Native Native -> Native -> IO MachineState
dictGet ms d k =
  case MS.lookup k d of
    Just n -> return $ setCode ms (ReturnLit n Nothing)
    Nothing -> throwIn ms $ DictKeyNotFound k

-- | __DICTPUT(d, k, v)
dictPut :: MachineState -> MS.Map Native Native -> Native -> Native -> IO MachineState
dictPut ms d k v =
  return $ setCode ms (ReturnLit (NativeDict $ MS.insert k v d) Nothing)

-- | __DICTDEL(d, k)
dictDel :: MachineState -> MS.Map Native Native -> Native -> IO MachineState
dictDel ms d k =
  return $ setCode ms (ReturnLit (NativeDict $ MS.delete k d) Nothing)

-- | __DICTENTRIES(d)
dictEntries :: MachineState -> MS.Map Native Native -> IO MachineState
dictEntries ms d = returnNatPairList ms (MS.assocs d)
