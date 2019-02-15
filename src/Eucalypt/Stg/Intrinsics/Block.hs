{-|
Module      : Eucalypt.Stg.Intrinsics.Block
Description : Block intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Block
  ( prune
  , pruneMerge
  , intrinsics
  ) where

import Data.Symbol
import Eucalypt.Stg.Address (allocate)
import Eucalypt.Stg.Error
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import qualified Eucalypt.Stg.Intrinsics.SymbolMap as SM
import Eucalypt.Stg.Loaders
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Syn


type IOSM = SM.InsOrdSymbolMap StgValue

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "PRUNE" 1 (invoke prune)
  , IntrinsicInfo "PRUNEMERGE" 2 (invoke pruneMerge)
  ]

-- | Utility to return a native list from a primitive function.
--
-- Allocates all links and then 'ReturnCon's back to caller.
returnPairList :: MachineState -> IOSM -> IO MachineState
returnPairList ms om = returnList ms $ (map snd . SM.toList) om

-- | Takes list of pairs and prunes such that later values for the
-- same key replace previous values, maintaining order of original
-- occurence.
--
prune :: MachineState -> Address -> IO MachineState
prune ms a = pruneSub ms SM.empty a >>= returnPairList ms



-- | The value of each pair in the ordered map is the pair itself
-- (allowing for simple reconstruction without allocation of the
-- pairs in the returned pair list.)
pruneSub ::
     MachineState
  -> IOSM
  -> Address
  -> IO IOSM
pruneSub ms om a = do
  cons <- readCons ms a
  case cons of
    Just (h, StgAddr t, _) -> do
      (k, _, _) <- kvtail ms h
      let om' = SM.insertWith const k h om
      pruneSub ms om' t
    Just (_, _, _) -> throwIn ms IntrinsicImproperList
    Nothing -> return om



-- | Similar to prune but accepts a function to use to combine values
-- where the key occurs twice (resulting from left and right blocks).
--
-- The combination thunk is allocated but not evaluated.
pruneMerge :: MachineState -> Address -> Address -> IO MachineState
pruneMerge ms xs cmb = do
  om <- pruneMergeSub cmb SM.empty xs
  returnPairList ms om
  where
    pruneMergeSub ::
         Address
      -> IOSM
      -> Address
      -> IO IOSM
    pruneMergeSub f om a = do
      cons <- readCons ms a
      case cons of
        Just (h, StgAddr t, _) -> do
          (k, StgAddr cdr, _) <- kvtail ms h
          let old = SM.lookup k om
          case old of
            Nothing -> pruneMergeSub f (SM.insert k h om) t
            Just o -> do
              (_, StgAddr oldcdr, _) <- kvtail ms o
              Just (oldval, _, _) <- readCons ms oldcdr
              Just (newval, _, _) <- readCons ms cdr
              combined <- combine k f newval oldval
              pruneMergeSub f (SM.insert k combined om) t
        Just (_, _, _) -> throwIn ms IntrinsicImproperList
        Nothing -> return om
    combine :: Symbol -> Address -> StgValue -> StgValue -> IO StgValue
    combine k f new old =
      let env = toVec [StgAddr f, old, new]
          cs = machineCallStack ms
       in do addr <-
               StgAddr <$>
               allocate
                 (Closure
                    (thunkn_ 3 $ appfn_ (L 0) [L 1, L 2])
                    env
                    cs
                    MetadataPassThrough)
             load ms (k, addr)
