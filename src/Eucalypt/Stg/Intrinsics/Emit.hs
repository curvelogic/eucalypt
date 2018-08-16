{-|
Module      : Eucalypt.Stg.Intrinsics.Emit
Description : Built-ins for emitting events from the STG evaluator
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Emit
  ( emitMappingStart
  , emitMappingEnd
  , emitSequenceStart
  , emitSequenceEnd
  , emitScalar
  ) where

import Data.Vector ((!))
import Eucalypt.Stg.Event
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Tags

emit :: MachineState -> Event -> IO MachineState
emit s e = send e >>= \s' -> return $ setCode s' (ReturnCon stgUnit mempty)
  where
    send = machineEmit s s

emitMappingStart :: MachineState -> ValVec -> IO MachineState
emitMappingStart s _ = emit s OutputMappingStart

emitMappingEnd :: MachineState -> ValVec -> IO MachineState
emitMappingEnd s _ = emit s OutputMappingEnd

emitSequenceStart :: MachineState -> ValVec -> IO MachineState
emitSequenceStart s _ = emit s OutputSequenceStart

emitSequenceEnd :: MachineState -> ValVec -> IO MachineState
emitSequenceEnd s _ = emit s OutputSequenceEnd

emitScalar :: MachineState -> ValVec -> IO MachineState
emitScalar s (ValVec xs) = do
  let (StgNat n) = xs ! 0
  (`setCode` ReturnLit n) <$> emit s (OutputScalar n)
