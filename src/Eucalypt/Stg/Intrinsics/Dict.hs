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

import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import qualified Data.Map.Strict as MS
import Data.Vector ((!))

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "EMPTYDICT" 0 emptyDict
  , IntrinsicInfo "DICTCONTAINSKEY" 2 dictContainsKey
  , IntrinsicInfo "DICTGET" 2 dictGet
  , IntrinsicInfo "DICTPUT" 2 dictPut
  , IntrinsicInfo "DICTDEL" 2 dictDel
  ]

-- | __EMPTYDICT
emptyDict :: MachineState -> ValVec -> IO MachineState
emptyDict ms _ = return $ setCode ms (ReturnLit (NativeDict MS.empty) Nothing)

-- | __DICTCONTAINSKEY(d, k)
dictContainsKey :: MachineState -> ValVec -> IO MachineState
dictContainsKey ms (ValVec args) = do
  let (StgNat (NativeDict d) _) = args ! 0
  let (StgNat k _) = args ! 1
  return $ setCode ms (ReturnLit (NativeBool $ k `MS.member` d) Nothing)

-- | __DICTGET(d, k)
dictGet :: MachineState -> ValVec -> IO MachineState
dictGet ms (ValVec args) = do
  let (StgNat (NativeDict d) _) = args ! 0
  let (StgNat k _) = args ! 1
  case MS.lookup k d of
    Just n -> return $ setCode ms (ReturnLit n Nothing)
    Nothing -> throwIn ms $ DictKeyNotFound k

-- | __DICTPUT(d, k, v)
dictPut :: MachineState -> ValVec -> IO MachineState
dictPut ms (ValVec args) = do
  let (StgNat (NativeDict d) _) = args ! 0
  let (StgNat k _) = args ! 1
  let (StgNat v _) = args ! 2
  return $ setCode ms (ReturnLit (NativeDict $ MS.insert k v d) Nothing)

-- | __DICTDEL(d, k)
dictDel :: MachineState -> ValVec -> IO MachineState
dictDel ms (ValVec args) = do
  let (StgNat (NativeDict d) _) = args ! 0
  let (StgNat k _) = args ! 1
  return $ setCode ms (ReturnLit (NativeDict $ MS.delete k d) Nothing)
