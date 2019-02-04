{-|
Module      : Eucalypt.Stg.Intrinsics.Set
Description : Basic set built-ins for the STG evaluator
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Set
  ( intrinsics
  ) where

import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Native
import Eucalypt.Stg.Machine
import qualified Data.Set as S
import Data.Sequence ((!?))

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "EMPTYSET" 0 emptySet
  , IntrinsicInfo "SETCONTAINS" 2 setContains
  , IntrinsicInfo "SETADD" 2 setAdd
  , IntrinsicInfo "SETREMOVE" 2 setRemove
  , IntrinsicInfo "SETMEMBERS" 2 setMembers
  ]

getSetAndKey
  :: MachineState -> ValVec -> IO (S.Set Native, Native)
getSetAndKey ms args = do
  ns <- getNatives ms args
  let (Just (NativeSet d)) = ns !? 0
  let (Just k) = ns !? 1
  return (d, k)


-- | __EMPTYSET
emptySet :: MachineState -> ValVec -> IO MachineState
emptySet ms _ = return $ setCode ms (ReturnLit (NativeSet S.empty) Nothing)

-- | __SETCONTAINS(s, e)
setContains :: MachineState -> ValVec -> IO MachineState
setContains ms args = do
  (s, n) <- getSetAndKey ms args
  returnBool ms $ n `S.member` s

-- | __SETADD(s, e)
setAdd :: MachineState -> ValVec -> IO MachineState
setAdd ms args = do
  (s, n) <- getSetAndKey ms args
  return $ setCode ms (ReturnLit (NativeSet $ S.insert n s) Nothing)

-- | __SETADD(s, e)
setRemove :: MachineState -> ValVec -> IO MachineState
setRemove ms args = do
  (s, n) <- getSetAndKey ms args
  return $ setCode ms (ReturnLit (NativeSet $ S.delete n s) Nothing)

-- | __SETMEMBERS(s)
setMembers :: MachineState -> ValVec -> IO MachineState
setMembers ms (ValVec args) = do
  let (Just (StgNat (NativeSet s) _)) = args !? 0
  returnNatList ms (S.toList s)
