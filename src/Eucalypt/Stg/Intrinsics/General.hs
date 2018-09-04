{-|
Module      : Eucalypt.Stg.Intrinsics.General
Description : General builtins for access to machine internals and state
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.General
  ( closed
  ) where

import Data.Vector ((!))
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine

-- | Is the heap object at the specified address a closed term?
--
-- Note that closed terms may return lambdas so it is necessary to
-- force evaluation to use this function to determine whether the
-- ultimate value of what's at the address is callable or not.
closed :: MachineState -> ValVec -> IO MachineState
closed ms (ValVec xs) =
  case xs ! 0 of
    (StgNat _) -> return $ setCode ms (ReturnLit $ NativeBool True)
    (StgAddr a) -> do
      obj <- peek a
      let ret =
            case obj of
              Closure {closureCode = LambdaForm {_bound = b}} -> b == 0
              PartialApplication {papArity = 0} -> True
              _ -> False
      return $ setCode ms (ReturnLit $ NativeBool ret)
