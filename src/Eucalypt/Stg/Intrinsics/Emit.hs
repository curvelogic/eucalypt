{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase, OverloadedStrings #-}
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

import Data.Symbol
import Eucalypt.Stg.Address (peek)
import Eucalypt.Stg.Error
import Eucalypt.Stg.Event
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import qualified Eucalypt.Stg.Intrinsics.SymbolMap as SM
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
emit s@MachineState {machineEmitHook = hook} e = do
  s' <-
    case hook of
      (Just f) -> f s e
      Nothing -> return s
  return $ (appendEvent e . setCode s') (ReturnCon stgUnit mempty Nothing)

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
emitScalar s x =
  case x of
    (StgNat n m) -> do
      event <- case m of
                 Just meta -> flip OutputScalar n <$> renderMeta s meta
                 Nothing -> return $ OutputScalar (RenderMetadata Nothing) n
      (`setCode` ReturnLit n Nothing) <$> emit s event
    (StgAddr _) -> error "Received address in emitScalar"

readValueFromHeapTail :: MachineState -> Address -> IO (Maybe StgValue)
readValueFromHeapTail ms addr =
  readCons ms addr >>= \case
    (Just (v, _, _)) -> return . Just $ v
    _ -> return Nothing

getValue :: MachineState -> Address -> Symbol -> IO (Maybe StgValue)
getValue ms a k = do
  cons <- readCons ms a
  case cons of
    (Just (StgAddr h, StgAddr t, _)) -> do
      Just (StgNat (NativeSymbol k') _, StgAddr v', _) <- readCons ms h
      if k' == k
        then readValueFromHeapTail ms v'
        else getValue ms t k
    _ -> return Nothing

-- | Assuming the address contains some form of block implementation
-- (representing metadata) excavate out a native value for the
-- specified key.
--
-- Gruesome.
excavate :: MachineState -> Symbol -> Address -> IO (Maybe Native)
excavate ms k a = do
  obj <- peek a
  val <-
    case obj of
      Closure { closureEnv = e
              , closureCode = LambdaForm {lamBody = (App (Con TagBlock) xs)}
              } -> do
        let (StgAddr lst) = (`index` 0) $ values (e, ms) $ nativeToValue <$> xs
        getValue ms lst k
      Closure { closureEnv = e
              , closureCode = LambdaForm {lamBody = (App (Con TagIOSMBlock) xs)}
              } ->
        case (`index` 0) $ values (e, ms) $ nativeToValue <$> xs of
          (StgNat (NativeDynamic iosm) _) -> SM.lookup k <$> cast ms iosm
          (StgAddr iosmAddr) -> do
            obj' <- peek iosmAddr
            case obj' of
              Closure {closureCode = LambdaForm {lamBody = (Atom (V (NativeDynamic iosm)))}} ->
                SM.lookup k <$> cast ms iosm
              _ -> return Nothing
          _ -> return Nothing
      Closure {closureCode = LambdaForm {lamBody = expr@(App (Con _) _)}} ->
        throwIn ms $ IntrinsicExpectedBlock expr
      Closure {closureCode = LambdaForm {..}} ->
        throwIn ms $ IntrinsicExpectedEvaluatedBlock lamBody
      BlackHole -> throwIn ms IntrinsicExpectedBlockFoundBlackHole
      PartialApplication {} ->
        throwIn ms IntrinsicExpectedBlockFoundPartialApplication
  case val of
    (Just (StgNat n _)) -> return $ Just n
    _ -> return Nothing

-- | Read 'RenderMetadata' out of the machine
renderMeta :: MachineState -> StgValue -> IO RenderMetadata
renderMeta ms (StgAddr a) = do
  tagValue <- excavate ms (intern "tag") a
  case tagValue of
    (Just  (NativeString tag)) -> return . RenderMetadata . Just $ tag
    _ -> return . RenderMetadata $ Nothing
renderMeta _ _ = error "Native metadata in emit intrinsics"
