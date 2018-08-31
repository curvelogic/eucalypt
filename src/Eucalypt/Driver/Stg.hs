{-|
Module      : Eucalypt.Driver.Stg
Description : Drive compilation, evaluation and render using STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Driver.Stg
  ( render
  , renderConduit
  , dumpStg
  ) where


import Conduit
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Eucalypt.Core.Syn (CoreExpr)
import Eucalypt.Driver.Options (EucalyptOptions(..))
import qualified Eucalypt.Stg.Compiler as C
import Eucalypt.Stg.Eval (run, step)
import Eucalypt.Stg.Event (Event(..))
import Eucalypt.Stg.Machine (MachineState(..))
import Eucalypt.Stg.StandardMachine
  ( dump
  , dumpEmission
  , initStandardMachineState
  , initDebugMachineState
  )
import Eucalypt.Stg.Syn (StgPretty(..), StgSyn)
import qualified Text.PrettyPrint as P

-- | Compile, install a render sink and run
render :: EucalyptOptions -> CoreExpr -> IO ()
render opts expr = do
  syn <- compile expr
  ms <- newMachine syn
  ms' <- run ms {machineEmit = emit}
  dump ms'
  where
    format = fromMaybe "yaml" (optionExportFormat opts)
    emit = selectRenderSink format
    newMachine = if optionDebug opts then debugMachine else machine

-- | Dump STG expression to stdout
dumpStg :: EucalyptOptions -> CoreExpr -> IO ()
dumpStg _opts expr = compile expr >>= putStrLn . P.render . prettify

-- | Compile Core to STG
compile :: CoreExpr -> IO StgSyn
compile expr = return $ C.compileForRender expr

-- | Instantiate the STG machine
machine :: StgSyn -> IO MachineState
machine = initStandardMachineState

-- | Instantiate the debug STG machine
debugMachine :: StgSyn -> IO MachineState
debugMachine s = putStrLn "DEBUG" >> initDebugMachineState s

-- | Select an emit function appropriate to the render format
selectRenderSink :: String -> MachineState -> Event -> IO MachineState
selectRenderSink _format = dumpEmission

-- | Build a conduit streaming pipeline where the machine generates
-- events and renderer processes them.
renderConduit :: EucalyptOptions -> CoreExpr -> IO ()
renderConduit opts expr = do
  syn <- compile expr
  ms <- newMachine syn
  runConduit $ machineSource ms .| renderPipeline format
  where
    format = fromMaybe "yaml" (optionExportFormat opts)
    newMachine =
      if optionDebug opts
        then debugMachine
        else machine

machineSource :: MachineState -> ConduitT () Event IO ()
machineSource = loop
  where
    loop s = do
      s' <- step s
      yieldMany $ machineEvents s'
      when (machineTerminated s') $ loop s'


renderPipeline :: String -> ConduitT Event Void IO ()
renderPipeline _format = mapM_C print
