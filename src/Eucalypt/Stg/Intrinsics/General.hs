{-|
Module      : Eucalypt.Stg.Intrinsics.General
Description : General builtins for access to machine internals and state
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.General
  ( intrinsics
  ) where

import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Address (peek)
import Eucalypt.Stg.Machine


intrinsics :: [IntrinsicInfo]
intrinsics = [IntrinsicInfo "SATURATED" 1 (invoke saturated)]

-- | Is the heap object at the specified address a saturated term?
-- i.e. does not require further arguments to be evaluated to a value
-- and can therefore be sensibly rendered
--
-- Note that saturated terms may return lambdas so it is necessary to
-- force evaluation to use this function to determine whether the
-- ultimate value of what's at the address is callable or not.
saturated :: MachineState -> StgValue -> IO MachineState
saturated ms v =
  case v of
    (StgNat _ _) -> returnBool ms True
    (StgAddr a) -> do
      obj <- peek a
      let ret =
            case obj of
              Closure {closureCode = LambdaForm {lamBound = b}} -> b == 0
              PartialApplication {papArity = 0} -> True
              _ -> False
      returnBool ms ret
