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
import Eucalypt.Stg.Value
import qualified Text.PrettyPrint as P


intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "INSPECT" 1 (invoke inspect)
  , IntrinsicInfo "GLOBAL" 1 (invoke global)
  ]

inspect :: MachineState -> StgValue -> IO MachineState
inspect ms v =
  case v of
    (StgAddr a) ->
      peek a >>= \ho ->
        return $
        setCode ms (ReturnLit (NativeString $ P.render $ prettify ho) Nothing)
    (StgNat n Nothing) ->
      return $
      setCode ms (ReturnLit (NativeString $ P.render $ prettify n) Nothing)
    (StgNat n (Just m)) ->
      return $
      setCode
        ms
        (ReturnLit
           (NativeString $
            P.render $ prettify n P.<+> (P.char '`' <> prettify m <> P.char '`'))
           Nothing)


global :: MachineState -> String -> IO MachineState
global ms nm = returnValue ms (retrieveGlobal ms nm)
