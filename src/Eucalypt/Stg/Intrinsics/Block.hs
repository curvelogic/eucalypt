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

import Control.Monad (foldM)
import Data.Symbol
import Eucalypt.Stg.Address (allocate)
import Eucalypt.Stg.Error
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import qualified Eucalypt.Stg.Intrinsics.SymbolMap as SM
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Type
import Eucalypt.Stg.Value

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


-- | Discard layer of Maybe throwing error if there's a nothing
orThrow :: MachineState -> StgError -> Maybe v -> IO v
orThrow ms e = maybe (throwIn ms e) return

-- | Discard layer of Maybe throwing error if there's a nothing
orThrowM :: MachineState -> IO StgError -> Maybe v -> IO v
orThrowM ms e = maybe (e >>= throwIn ms) return



-- | Takes list of pairs and prunes such that later values for the
-- same key replace previous values, maintaining order of original
-- occurence.
--
prune :: MachineState -> Address -> IO MachineState
prune ms a = do
  vals <- scrape ms (StgAddr a) >>= orThrowM ms improperList
  keys <- traverse toKey vals
  foldM ins SM.empty (zip keys vals) >>= returnPairList ms
  where
    ins :: IOSM -> (Symbol, StgValue) -> IO IOSM
    ins om (k, v) = SM.insertM k v om
    toKey kv =
      fst <$> (scrape ms kv >>= orThrow ms badPair :: IO (Symbol, StgValue))
    improperList = do
      obtainedType <- classify $ StgAddr a
      return $
        TypeMismatch
          { context = "Failed to read list from memory"
          , expected = [listType]
          , obtained = [obtainedType]
          , obtainedValues = [Just $ StgAddr a]
          }
    badPair = BadPair $ StgAddr a

-- | Stores the kv pair values as values of a symbol map keyed by the
-- the symbol key. (The values are the entire pair to preserve
-- fidelity of metadata where possible.)
--
-- TODO: recombination should honour metadata
toSymbolMap :: MachineState -> Address -> [StgValue] -> IO IOSM
toSymbolMap ms cmbAddr = foldM ins SM.empty
  where
    ins :: IOSM -> StgValue -> IO IOSM
    ins om kv =
      (scrape ms kv :: IO (Maybe (Symbol, StgValue))) >>=
      orThrow ms (BadPair kv) >>= \(k, _) -> SM.insertWithM merge k kv om
    -- | create a new pair from the previous two
    merge :: StgValue -> StgValue -> IO StgValue
    merge new old = do
      oldPair <- scrape ms old :: IO (Maybe [StgValue])
      newPair <- scrape ms new :: IO (Maybe [StgValue])
      case (oldPair, newPair) of
        (Just (k:o:_), Just (_:n:_)) -> do
          key <- scrape ms k :: IO (Maybe Symbol)
          case key of
            Just keySym -> combine keySym cmbAddr n o
            Nothing -> do
              typeObtained <- classify k
              throwIn ms $
                TypeMismatch
                  { context =
                      "Non-symbol key in key/value pair while pruning block"
                  , expected = [TypeSymbol]
                  , obtained = [typeObtained]
                  , obtainedValues = [Just k]
                  }
        (Nothing, Just _) -> throwIn ms $ BadPair old
        _ -> throwIn ms $ BadPair new
      -- | create a new value from the old ones
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



-- | Similar to prune but accepts a function to use to combine values
-- where the key occurs twice (resulting from left and right blocks).
--
-- The combination thunk is allocated but not evaluated.
pruneMerge :: MachineState -> Address -> Address -> IO MachineState
pruneMerge ms xs cmb =
  scrape ms (StgAddr xs) >>= orThrowM ms mismatch >>= toSymbolMap ms cmb >>=
  returnPairList ms
  where
    mismatch = do
      obtainedType <- classify $ StgAddr xs
      return $
        TypeMismatch
          { context = "Found bad key/value pair while merging lists"
          , expected = [listType]
          , obtained = [obtainedType]
          , obtainedValues = [Just $ StgAddr xs]
          }
