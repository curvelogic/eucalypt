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

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "EMPTYSET" 0 (invoke emptySet)
  , IntrinsicInfo "SETCONTAINS" 2 (invoke setContains)
  , IntrinsicInfo "SETADD" 2 (invoke setAdd)
  , IntrinsicInfo "SETREMOVE" 2 (invoke setRemove)
  , IntrinsicInfo "SETMEMBERS" 2 (invoke setMembers)
  ]

-- | __EMPTYSET
emptySet :: MachineState -> IO MachineState
emptySet ms = return $ setCode ms (ReturnLit (NativeSet S.empty) Nothing)

-- | __SETCONTAINS(s, e)
setContains :: MachineState -> S.Set Native -> Native -> IO MachineState
setContains ms s n = returnBool ms $ n `S.member` s

-- | __SETADD(s, e)
setAdd :: MachineState -> S.Set Native -> Native -> IO MachineState
setAdd ms s n =
  return $ setCode ms (ReturnLit (NativeSet $ S.insert n s) Nothing)

-- | __SETADD(s, e)
setRemove :: MachineState -> S.Set Native -> Native -> IO MachineState
setRemove ms s n =
  return $ setCode ms (ReturnLit (NativeSet $ S.delete n s) Nothing)

-- | __SETMEMBERS(s)
setMembers :: MachineState -> S.Set Native -> IO MachineState
setMembers ms s = returnNatList ms (S.toList s)
