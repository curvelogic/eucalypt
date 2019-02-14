{-|
Module      : Eucalypt.Stg.Intrinsics.Debug
Description : Intrinsics for troubleshooting
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Debug
  ( intrinsics
  ) where

import Eucalypt.Stg.Address
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Stg.Pretty
import qualified Text.PrettyPrint as P


intrinsics :: [IntrinsicInfo]
intrinsics = [IntrinsicInfo "INSPECT" 1 (invoke inspect)]

inspect :: MachineState -> StgValue -> IO MachineState
inspect ms v =
  case v of
    (StgAddr a) ->
      peek a >>= \ho ->
        return $
        setCode ms (ReturnLit (NativeString $ P.render $ prettify ho) Nothing)
    (StgNat n _) ->
      return $
      setCode ms (ReturnLit (NativeString $ P.render $ prettify n) Nothing)
