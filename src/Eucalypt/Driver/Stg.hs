{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
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
  , runHeadless
  , dumpStg
  ) where


import Conduit hiding (throwM)
import Control.Exception.Safe (IOException, MonadCatch, catch, handle, throwM)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Eucalypt.Core.Syn (CoreExpr)
import Eucalypt.Driver.Options (EucalyptOptions(..))
import qualified Eucalypt.Render.Json as Json
import qualified Eucalypt.Render.Text as Text
import qualified Eucalypt.Render.Yaml as Yaml
import qualified Eucalypt.Stg.Compiler as C
import Eucalypt.Reporting.Error
import Eucalypt.Stg.Error
import Eucalypt.Stg.Eval (step)
import Eucalypt.Stg.Event (Event(..))
import Eucalypt.Stg.Machine (MachineState(..))
import Eucalypt.Stg.Pretty
import Eucalypt.Stg.StandardMachine
  ( initDebugMachineState
  , initStandardMachineState
  )
import Eucalypt.Stg.Syn (StgSyn)
import qualified Text.PrettyPrint as P

-- | Dump STG expression to stdout
dumpStg :: EucalyptOptions -> CoreExpr -> IO ()
dumpStg _opts expr = compile expr >>= putStrLn . P.render . prettify



-- | Compile Core to STG
compile :: MonadIO m => CoreExpr -> m StgSyn
compile expr = return $ C.compileForRender expr



-- | Instantiate the STG machine
machine :: MonadIO m => StgSyn -> m MachineState
machine = initStandardMachineState



-- | Instantiate the debug STG machine
debugMachine :: MonadIO m => StgSyn -> m MachineState
debugMachine = initDebugMachineState


-- | Useful for comparing speed of conduit render pipelines with raw
--  evaluation.
runHeadless :: (MonadUnliftIO m, MonadIO m, MonadThrow m, MonadCatch m)
  => EucalyptOptions
  -> CoreExpr
  -> m ()
runHeadless opts expr = do
  syn <- compile expr
  ms <- newMachine syn
  loop ms
  where
    newMachine =
      if optionDebug opts
        then debugMachine
        else machine
    loop s = do
      s' <-
        step s `catch`
        (\(e :: IOException) ->
           throwM $ StgException (IOSystem e) (machineCallStack s))
      let !_events = reverse $ machineEvents s'
          terminated = machineTerminated s'
        in unless terminated $ loop s'



-- | Build a conduit streaming pipeline where the machine generates
-- events and renderer processes them.
renderConduit ::
     (MonadUnliftIO m, MonadIO m, MonadThrow m, MonadCatch m)
  => EucalyptOptions
  -> CoreExpr
  -> m BS.ByteString
renderConduit opts expr = handle handler $ do
  syn <- compile expr
  ms <- newMachine syn
  runConduitRes $ machineSource ms .| renderPipeline format
  where
    format = fromMaybe "yaml" (optionExportFormat opts)
    newMachine =
      if optionDebug opts
        then debugMachine
        else machine
    handler :: MonadCatch m => StgException -> m BS.ByteString
    handler e = throwM $ Execution e



-- | Step through the machine yielding events via the conduit pipeline
-- at each stage
machineSource ::
     (MonadUnliftIO m, MonadResource m, MonadIO m, MonadThrow m)
  => MachineState
  -> ConduitT () Event m ()
machineSource ms = do
  yield OutputStreamStart
  yield OutputDocumentStart
  loop ms
  yield OutputDocumentEnd
  yield OutputStreamEnd
  where
    loop s = do
      s' <-
        step s `catchC`
        (\(e :: IOException) ->
           throwM $ StgException (IOSystem e) (machineCallStack s))
      let events = reverse $ machineEvents s'
       in yieldMany events
      unless (machineTerminated s') $ loop s'


-- | Select an appropriate render pipeline based on the requested
-- format
renderPipeline ::
     (MonadResource m) => String -> ConduitT Event Void m BS.ByteString
renderPipeline "json" = Json.pipeline
renderPipeline "text" = Text.pipeline
renderPipeline _ = Yaml.pipeline
