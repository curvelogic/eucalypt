{-|
Module      : Eucalypt.Stg.StandardMachine
Description : Helpers for standard machine configurations
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.StandardMachine where

import Eucalypt.Stg.Event
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Globals
import Eucalypt.Stg.Machine
import qualified Text.PrettyPrint as P


-- | Initialise machine state with the standard global defs.
initStandardMachineState :: StgSyn -> IO MachineState
initStandardMachineState s = initMachineState s standardGlobals

-- | A debug dump to use as machine's trace function
dump :: MachineState -> IO ()
dump ms = putStrLn $ P.render $ prettify ms

-- | An emit function to use for debugging
dumpEmission :: MachineState -> Event -> IO MachineState
dumpEmission ms@MachineState {machineDebugEmitLog = es} e =
  return ms {machineDebugEmitLog = es ++ [e]}

-- | Initialise machine state with a trace function that dumps state
-- every step
initDebugMachineState :: StgSyn -> IO MachineState
initDebugMachineState stg = do
  ms <- initStandardMachineState stg
  return $ ms {machineTrace = dump, machineEmit = dumpEmission}
