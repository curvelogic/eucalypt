{-# LANGUAGE LambdaCase #-}
{-|
Module      : Eucalypt.Stg.Intrinsics.Emit
Description : Built-ins for emitting events from the STG evaluator
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Emit
  ( intrinsics
  ) where

import qualified Data.HashMap.Strict.InsOrd as OM
import Eucalypt.Stg.Address (peek)
import Eucalypt.Stg.Event
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Block (pruneBlockToMap)
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "EMIT{" 0 emitMappingStart
  , IntrinsicInfo "EMIT}" 0 emitMappingEnd
  , IntrinsicInfo "EMIT[" 0 emitSequenceStart
  , IntrinsicInfo "EMIT]" 0 emitSequenceEnd
  , IntrinsicInfo "EMITx" 1 (invoke emitScalar)
  , IntrinsicInfo "EMIT0" 0 emitNull
  , IntrinsicInfo "EMITT" 0 emitTrue
  , IntrinsicInfo "EMITF" 0 emitFalse
  ]


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

emitTrue :: MachineState -> ValVec -> IO MachineState
emitTrue s _ = emit s OutputTrue

emitFalse :: MachineState -> ValVec -> IO MachineState
emitFalse s _ = emit s OutputFalse

-- | This assumes that all render-relevant metadata has been forced to
-- native values.
emitScalar :: MachineState -> StgValue -> IO MachineState
emitScalar s x = do
  let (StgNat n m) = x
  event <-
    case m of
      Just meta -> flip OutputScalar n <$> renderMeta s meta
      Nothing -> return $ OutputScalar (RenderMetadata Nothing) n
  (`setCode` ReturnLit n Nothing) <$> emit s event

readString :: MachineState -> Address -> IO (Maybe String)
readString _ms addr =
  peek addr >>= \case
    Closure {closureCode = LambdaForm {lamBody = (Atom (V (NativeString s)))}} ->
      return . Just $ s
    _ -> return Nothing

readStringFromHeapTail :: MachineState -> Address -> IO (Maybe String)
readStringFromHeapTail ms addr =
  readCons ms addr >>= \case
    (Just (StgNat (NativeString s) _, _)) -> return . Just $ s
    (Just (StgAddr a, _)) -> readString ms a
    _ -> return Nothing

-- | Read 'RenderMetadata' out of the machine
renderMeta :: MachineState -> StgValue -> IO RenderMetadata
renderMeta ms (StgAddr a) = do
  kvs <- pruneBlockToMap ms mempty a
  case OM.lookup "tag" kvs of
    (Just (StgNat (NativeString tag) _)) -> return . RenderMetadata . Just $ tag
    (Just (StgAddr addr)) -> RenderMetadata <$> readStringFromHeapTail ms addr
    _ -> return . RenderMetadata $ Nothing
renderMeta _ _ = error "Native metadata in emit intrinsics"
