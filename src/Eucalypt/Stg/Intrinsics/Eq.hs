{-|
Module      : Eucalypt.Stg.Intrinsics.Eq
Description : Intrinsics for equality
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Eq where

import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Native
import Eucalypt.Stg.Machine
import Data.Sequence ((!?))

toNative :: StgValue -> Maybe Native
toNative (StgNat n _) = Just n
toNative _ = Nothing

natEq :: MachineState -> ValVec -> IO MachineState
natEq ms (ValVec xs) =
  returnBool ms $
  case (xs !? 0 >>= toNative, xs !? 1 >>= toNative) of
    (Just l, Just r) -> l == r
    _ -> False
