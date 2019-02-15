{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Eucalypt.Stg.Loaders
Description : Utils for loading haskell values into STG machine memory
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Loaders where

import Data.Foldable (foldrM)
import Data.Symbol
import Eucalypt.Stg.Address
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

consVals :: StgValue -> StgValue -> IO StgValue
consVals a as =
  StgAddr <$>
  allocate (Closure consConstructor (toVec [a, as]) mempty MetadataPassThrough)

class Loadable a where
  load :: MachineState -> a -> IO StgValue

-- instance Loadable String where
--   load _ s = return (StgNat (NativeString s) Nothing)

instance Loadable Symbol where
  load _ s = return (StgNat (NativeSymbol s) Nothing)

instance Loadable Native where
  load _ms n = return (StgNat n Nothing)

instance Loadable StgValue where
  load _ms = return

instance (Loadable a, Loadable b) => Loadable (a, b) where
  load ms (k, v) = do
    k' <- load ms k
    v' <- load ms v
    consVals v' (retrieveGlobal ms "KNIL") >>= consVals k'

instance Loadable a => Loadable [a] where
  load ms vs =
    traverse (load ms) vs >>= foldrM consVals (retrieveGlobal ms "KNIL")
