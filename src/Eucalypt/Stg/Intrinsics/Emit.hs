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
  , emitNull
  , emitSequenceStart
  , emitSequenceEnd
  , emitScalar
  ) where

import qualified Data.HashMap.Strict.InsOrd as OM
import Data.Vector ((!))
import Eucalypt.Stg.Event
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Intrinsics.Block (pruneToMap)

emit :: MachineState -> Event -> IO MachineState
emit s e =
  send e >>= \s' ->
    return $ (appendEvent e . setCode s') (ReturnCon stgUnit mempty Nothing)
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

emitNull :: MachineState -> ValVec -> IO MachineState
emitNull s _ = emit s OutputNull

-- | This assumes that all render-relevant metadata has been forced to
-- native values.
emitScalar :: MachineState -> ValVec -> IO MachineState
emitScalar s (ValVec xs) = do
  let (StgNat n m) = xs ! 0
  event <-
    case m of
      Just meta -> flip OutputScalar n <$> renderMeta s meta
      Nothing -> return $ OutputScalar (RenderMetadata Nothing) n
  (`setCode` ReturnLit n Nothing) <$> emit s event

renderMeta :: MachineState -> StgValue -> IO RenderMetadata
renderMeta ms (StgAddr a) = do
  kvs <- pruneToMap ms mempty a
  case OM.lookup "tag" kvs of
    (Just (StgNat (NativeString tag) _)) -> return . RenderMetadata . Just $ tag
    _ -> return . RenderMetadata $ Nothing
renderMeta _ _ = error "Native metadata in emit intrinsics"
