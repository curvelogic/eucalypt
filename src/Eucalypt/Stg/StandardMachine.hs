{-|
Module      : Eucalypt.Stg.StandardMachine
Description : Helpers for standard machine configurations
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.StandardMachine where

import Control.Monad.IO.Class
import qualified Data.Map as Map
import Eucalypt.Stg.Event
import Eucalypt.Stg.Syn
import Eucalypt.Stg.GlobalInfo (globalNames)
import Eucalypt.Stg.Globals (globals)
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Pretty
import qualified Text.PrettyPrint as P

-- | Lambda forms representing the global environment
globalLambdaForms :: Vec LambdaForm
globalLambdaForms = toVec $ map (lfs Map.!) globalNames
  where
    lfs = foldl add Map.empty globals
    add m (k, lf) = Map.insert k lf m

-- | Initialise machine state with the standard global defs.
initStandardMachineState :: MonadIO m => StgSyn -> m MachineState
initStandardMachineState s = initMachineState s globalLambdaForms

-- | A debug dump to use as machine's trace function
dump :: MachineState -> IO ()
dump ms = putStrLn $ P.render $ prettify ms

-- | An emit function to use for debugging
dumpEmission :: MachineState -> Event -> IO MachineState
dumpEmission ms@MachineState {machineDebugEmitLog = es} e =
  return ms {machineDebugEmitLog = es ++ [e]}

-- | Initialise machine state with a trace function that dumps state
-- every step
initDebugMachineState :: MonadIO m => StgSyn -> m MachineState
initDebugMachineState stg = do
  ms <- initStandardMachineState stg
  return $
    ms {machineTrace = dump, machineEmit = dumpEmission, machineDebug = True}
