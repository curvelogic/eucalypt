{-|
Module      : Eucalypt.Driver.Stg
Description : Drive compilation, evaluation and render using STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Driver.Stg
  ( renderConduit
  , dumpStg
  ) where


import Conduit
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Eucalypt.Core.Syn (CoreExpr)
import Eucalypt.Driver.Options (EucalyptOptions(..))
import qualified Eucalypt.Render.Yaml as Yaml
import qualified Eucalypt.Stg.Compiler as C
import Eucalypt.Stg.Eval (step)
import Eucalypt.Stg.Event (Event(..))
import Eucalypt.Stg.Machine (MachineState(..))
import Eucalypt.Stg.StandardMachine
  ( initDebugMachineState
  , initStandardMachineState
  )
import Eucalypt.Stg.Syn (StgPretty(..), StgSyn)
import qualified Text.PrettyPrint as P

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
debugMachine = initDebugMachineState



-- | Build a conduit streaming pipeline where the machine generates
-- events and renderer processes them.
renderConduit :: EucalyptOptions -> CoreExpr -> IO BS.ByteString
renderConduit opts expr = do
  syn <- compile expr
  ms <- newMachine syn
  runConduitRes $ machineSource ms .| renderPipeline format
  where
    format = fromMaybe "yaml" (optionExportFormat opts)
    newMachine =
      if optionDebug opts
        then debugMachine
        else machine



-- | Step through the machine yielding events via the conduit pipeline
-- at each stage
machineSource ::
  (MonadResource m, MonadIO m, MonadThrow m) => MachineState -> ConduitT () Event m ()
machineSource ms = do
  yield OutputStreamStart
  yield OutputDocumentStart
  loop ms
  yield OutputDocumentEnd
  yield OutputStreamEnd
  where
    loop s = do
      s' <- step s
      yieldMany $ machineEvents s'
      unless (machineTerminated s') $ loop s'



-- | Select an appropriate render pipeline based on the requested
-- format
renderPipeline ::
     (MonadResource m) => String -> ConduitT Event Void m BS.ByteString
renderPipeline _format = Yaml.pipeline
