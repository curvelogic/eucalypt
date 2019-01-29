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
  , pruneBlockToMap
  ) where

import Control.Monad (foldM)
import Data.Sequence ((!?))
import Eucalypt.Stg.Address (allocate)
import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Machine
import qualified Data.HashMap.Strict.InsOrd as OM
import Eucalypt.Stg.Intrinsics.Common


-- | Utility to return a native list from a primitive function.
--
-- Allocates all links and then 'ReturnCon's back to caller.
returnPairList :: MachineState -> OM.InsOrdHashMap String StgValue -> IO MachineState
returnPairList ms om = do
  nilAddr <- globalAddress ms "KNIL"
  let pairs = (map snd . OM.toList) om
   in case pairs of
        [] -> return $ setCode ms (ReturnCon stgNil mempty Nothing)
        (h:t) -> do
          tv <- foldM flipCons nilAddr (reverse t)
          return $ setCode ms (ReturnCon stgCons (toValVec [h, tv]) Nothing)


-- | Takes list of pairs and prunes such that later values for the
-- same key replace previous values, maintaining order of original
-- occurence.
--
prune :: MachineState -> ValVec -> IO MachineState
prune ms (ValVec xs) =
  let (Just (StgAddr a)) = xs !? 0
   in pruneSub ms OM.empty a >>= returnPairList ms



-- | Inspect a 'StgValue' to turn it into a pair of symbol and
-- value-tail
kv :: MachineState -> StgValue -> IO (String, StgValue)
kv ms (StgAddr addr) = do
  pair <- readCons ms addr
  case pair of
    Just (StgNat (NativeSymbol s) _, t) -> return (s, t)
    _ -> throwIn ms IntrinsicBadPair
kv ms (StgNat n _) = throwIn ms $ IntrinsicExpectedListFoundNative n



-- | The value of each pair in the ordered map is the pair itself
-- (allowing for simple reconstruction without allocation of the
-- pairs in the returned pair list.)
pruneSub ::
     MachineState
  -> OM.InsOrdHashMap String StgValue
  -> Address
  -> IO (OM.InsOrdHashMap String StgValue)
pruneSub ms om a = do
  cons <- readCons ms a
  case cons of
    Just (h, StgAddr t) -> do
      (k, _) <- kv ms h
      let om' = OM.insertWith const k h om
      pruneSub ms om' t
    Just (_, _) -> throwIn ms IntrinsicImproperList
    Nothing -> return om



-- | Read a block from the machine into a Haskell map. The value of
-- each pair in this ordered map is the value of the kv pair. Values
-- are not evaluated.
pruneBlockToMap ::
     MachineState
  -> OM.InsOrdHashMap String StgValue
  -> Address
  -> IO (OM.InsOrdHashMap String StgValue)
pruneBlockToMap ms om a = do
  elements <- readBlock ms a
  case elements of
    (StgAddr e) -> pruneToMap ms om e
    (StgNat n _) -> throwIn ms $ IntrinsicExpectedBlockFoundNative n



pruneToMap ::
     MachineState
  -> OM.InsOrdHashMap String StgValue
  -> Address
  -> IO (OM.InsOrdHashMap String StgValue)
pruneToMap ms om a = do
  cons <- readCons ms a
  case cons of
    Just (h, StgAddr t) -> do
      (k, v) <- kv ms h
      let om' = OM.insertWith const k v om
      pruneToMap ms om' t
    Just (_, _) -> throwIn ms IntrinsicImproperList
    Nothing -> return om



-- | Similar to prune but accepts a function to use to combine values
-- where the key occurs twice (resulting from left and right blocks).
--
-- The combination thunk is allocated but not evaluated.
pruneMerge :: MachineState -> ValVec -> IO MachineState
pruneMerge ms (ValVec xs) =
  let (Just (StgAddr a)) = xs !? 0
      (Just (StgAddr f)) = xs !? 1
   in do om <- pruneMergeSub f OM.empty a
         returnPairList ms om
  where
    pruneMergeSub ::
         Address
      -> OM.InsOrdHashMap String StgValue
      -> Address
      -> IO (OM.InsOrdHashMap String StgValue)
    pruneMergeSub f om a = do
      cons <- readCons ms a
      case cons of
        Just (h, StgAddr t) -> do
          (k, StgAddr cdr) <- kv ms h
          let old = OM.lookup k om
          case old of
            Nothing -> pruneMergeSub f (OM.insert k h om) t
            Just o -> do
              (_, StgAddr oldcdr) <- kv ms o
              Just (oldval, _) <- readCons ms oldcdr
              Just (newval, _) <- readCons ms cdr
              combined <- combine k f newval oldval
              pruneMergeSub f (OM.insert k combined om) t
        Just (_, _) -> throwIn ms IntrinsicImproperList
        Nothing -> return om
    combine :: String -> Address -> StgValue -> StgValue -> IO StgValue
    combine k f new old =
      let env = toValVec [StgAddr f, old, new]
          cs = machineCallStack ms
       in do addr <-
               StgAddr <$>
               allocate
                 (Closure
                    (thunkn_ 3 $ appfn_ (Local 0) [Local 1, Local 2])
                    env
                    cs
                    MetadataPassThrough)
             t <-
               do nilAddr <- globalAddress ms "KNIL"
                  StgAddr <$>
                    allocate
                      (Closure
                         consConstructor
                         (toValVec [addr, nilAddr])
                         cs
                         MetadataPassThrough)
             StgAddr <$>
               allocate
                 (Closure
                    consConstructor
                    (toValVec [StgNat (NativeSymbol k) Nothing, t])
                    cs
                    MetadataPassThrough)
