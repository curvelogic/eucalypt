{-|
Module      : Eucalypt.Stg.Intrinsics.Eq
Description : Intrinsics for equality
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Eq where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Data.Vector ((!))

asNative :: StgValue -> Maybe Native
asNative (StgNat n) = Just n
asNative _ = Nothing

natEq :: MachineState -> ValVec -> IO MachineState
natEq ms (ValVec xs) =
  (return . setCode ms . ReturnLit . NativeBool) $
  case (asNative $ xs ! 0, asNative $ xs ! 1) of
    (Just l, Just r) -> l == r
    _ -> False