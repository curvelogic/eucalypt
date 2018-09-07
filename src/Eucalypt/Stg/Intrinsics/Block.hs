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
  ) where

import Control.Monad (foldM)
import Data.Vector ((!))
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
  nilAddr <- StgAddr <$> allocClosure mempty ms (pc0_ nilConstructor)
  let pairs = (map snd . OM.toList) om
   in case pairs of
        [] -> return $ setCode ms (ReturnCon stgNil mempty)
        (h:t) -> do
          tv <- foldM flipCons nilAddr (reverse t)
          return $ setCode ms (ReturnCon stgCons (toValVec [h, tv]))



-- | Takes list of pairs and prunes such that later values for the
-- same key replace previous values, maintaining order of original
-- occurence.
prune :: MachineState -> ValVec -> IO MachineState
prune ms (ValVec xs) =
  let (StgAddr a) = xs ! 0
   in do om <- pruneSub OM.empty a
         returnPairList ms om
  where
    pruneSub ::
         OM.InsOrdHashMap String StgValue
      -> Address
      -> IO (OM.InsOrdHashMap String StgValue)
    pruneSub om a = do
      cons <- readCons ms a
      case cons of
        Just (h, StgAddr t) -> do
          (k, _) <- kv h
          let om' = OM.insertWith const k h om
          pruneSub om' t
        Just (_, _) -> throwIn ms IntrinsicImproperList
        Nothing -> return om
    kv (StgAddr a) = do
      pair <- readCons ms a
      case pair of
        Just (StgNat (NativeSymbol s), t) -> return (s, t)
        _ -> throwIn ms $ IntrinsicBadPair $ show pair
    kv _ = throwIn ms IntrinsicExpectedList
