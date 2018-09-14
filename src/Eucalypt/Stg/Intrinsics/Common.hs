{-# LANGUAGE LambdaCase #-}
{-|
Module      : Eucalypt.Stg.Intrinsics.Common
Description : Common utilities for intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Common where

import Control.Monad (foldM)
import qualified Data.Vector as V
import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Machine


flipCons :: StgValue -> StgValue -> IO StgValue
flipCons as a =
  StgAddr <$>
  allocate (Closure consConstructor (toValVec [a, as]) mempty MetadataPassThrough)


-- | Utility to return a native list from a primitive function.
--
-- Allocates all links and then 'ReturnCon's back to caller.
returnNatList :: MachineState -> [Native] -> IO MachineState
returnNatList ms ns = do
  nilAddr <- StgAddr <$> allocClosure mempty ms (pc0_ nilConstructor)
  let natAddrs = map (`StgNat` Nothing) ns
  if null natAddrs
    then return $ setCode ms (ReturnCon stgNil mempty Nothing)
    else do
      let headAddr = head natAddrs
      tailAddr <- foldM flipCons nilAddr (reverse $ tail natAddrs)
      return $ setCode ms (ReturnCon stgCons (toValVec [headAddr, tailAddr]) Nothing)


-- | Utility to read a list from the machine into a native haskell
-- list for a primitive function.
readNatList :: MachineState -> Address -> IO [Native]
readNatList ms addr = do
  obj <- peek addr
  case obj of
    Closure {closureCode = lf, closureEnv = e} ->
      case lf of
        LambdaForm {_body = (App (Con t) xs)}
          | t == stgCons -> do
            (StgNat h _) <- val e ms (V.head xs)
            (StgAddr a) <- val e ms (xs V.! 1)
            (h :) <$> readNatList ms a
        LambdaForm {_body = (App (Con t) _)}
          | t == stgNil -> return []
        _ -> throwIn ms IntrinsicExpectedNativeList
    _ -> throwIn ms IntrinsicExpectedNativeList


-- | Read native list from machine state where head is currently in a
-- ReturnCon form in the code.
readNatListReturn :: MachineState -> IO [Native]
readNatListReturn ms =
  case ms of
    MachineState {machineCode = (ReturnCon c (ValVec xs) Nothing)}
      | c == stgCons -> do
        let (StgNat h _) = V.head xs
        let (StgAddr t) = xs V.! 1
        (h :) <$> readNatList ms t
      | c == stgNil -> return []
    _ -> throwIn ms IntrinsicExpectedNativeList


-- | Read a list of strings from machine into native haskell list
readStrList :: MachineState -> Address -> IO [String]
readStrList ms addr = readNatList ms addr >>= traverse convert
  where
    convert (NativeString s) = return s
    convert _ = throwIn ms IntrinsicExpectedStringList

-- | Read a list of strings from machine to native haskell list where
-- head of list is currently in a ReturnCon form
readStrListReturn :: MachineState -> IO [String]
readStrListReturn ms = readNatListReturn ms >>= traverse convert
  where
    convert (NativeString s) = return s
    convert _ = throwIn ms IntrinsicExpectedStringList


-- | Utility to read a list of pairs from the machine into a native
-- haskell list for an intrinsic function.
readPairList :: MachineState -> Address -> IO [(String, StgValue)]
readPairList ms addr = do
  cons <- readCons ms addr
  case cons of
    Just (h, StgAddr t) -> do
      (k, cdr) <- kv h
      ((k, cdr) :) <$> readPairList ms t
    Just (_, _) -> throwIn ms IntrinsicImproperList
    Nothing -> return []
  where
    kv (StgAddr a) = do
      pair <- readCons ms a
      case pair of
        Just (StgNat (NativeSymbol s) _, t) -> return (s, t)
        _ -> throwIn ms $ IntrinsicBadPair $ show pair
    kv _ = throwIn ms IntrinsicExpectedList



readCons :: MachineState -> Address -> IO (Maybe (StgValue, StgValue))
readCons ms addr =
  peek addr >>= \case
    Closure {closureCode = lf, closureEnv = e} ->
      case lf of
        LambdaForm {_body = (App (Con t) xs)}
          | t == stgCons -> do
            h' <- val e ms (V.head xs)
            t' <- val e ms (xs V.! 1)
            return $ Just (h', t')
        LambdaForm {_body = (App (Con t) _)}
          | t == stgNil -> return Nothing
        _ -> throwIn ms $ IntrinsicExpectedEvaluatedList (show (_body lf))
    _ -> throwIn ms IntrinsicExpectedList
