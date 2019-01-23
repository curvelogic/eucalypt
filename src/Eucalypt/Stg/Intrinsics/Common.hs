{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns #-}
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
import Data.Foldable (toList)
import Data.List (intercalate)
import qualified Data.Sequence as Seq
import Data.Scientific (Scientific, floatingOrInteger)
import Data.Sequence (Seq)
import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Machine

type IntrinsicFunction = MachineState -> ValVec -> IO MachineState

isNative :: StgValue -> Bool
isNative (StgNat _ _) = True
isNative _ = False

asNative :: StgValue -> Native
asNative (StgNat n _) = n
asNative _ = error "Not a native"

getNatives :: MachineState -> ValVec -> IO (Seq.Seq Native)
getNatives ms (ValVec v) =
  if all isNative v
    then return $ fmap asNative v
    else throwIn ms NonNativeStgValue


flipCons :: StgValue -> StgValue -> IO StgValue
flipCons as a =
  StgAddr <$>
  allocate (Closure consConstructor (toValVec [a, as]) mempty MetadataPassThrough)


-- | Utility to return a native list from an intrinsic.
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

-- | Utility to return a list of pairs of natives from an intrinsic.
--
-- Allocates all links and then 'ReturnCon's back to caller.
returnNatPairList :: MachineState -> [(Native, Native)] -> IO MachineState
returnNatPairList ms ns = do
  nilAddr <- StgAddr <$> allocClosure mempty ms (pc0_ nilConstructor)
  pairAddrs <- traverse (allocPair nilAddr) ns
  if null pairAddrs
    then return $ setCode ms (ReturnCon stgNil mempty Nothing)
    else do
      let headAddr = head pairAddrs
      tailAddr <- foldM flipCons nilAddr (reverse $ tail pairAddrs)
      return $
        setCode ms (ReturnCon stgCons (toValVec [headAddr, tailAddr]) Nothing)
  where
    allocPair nil (k, v) =
      foldM flipCons nil [StgNat v Nothing, StgNat k Nothing]

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
            (StgNat h _) <- val e ms (xs `Seq.index` 0)
            (StgAddr a) <- val e ms (xs `Seq.index` 1)
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
        let (Just (StgNat h _)) = xs Seq.!? 0
        let (Just (StgAddr t)) = xs Seq.!? 1
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
        _ -> throwIn ms IntrinsicBadPair
    kv (StgNat n _) = throwIn ms $ IntrinsicExpectedListFoundNative n



-- | Assuming the specified address is a Cons cell, return head and tail.
readCons :: MachineState -> Address -> IO (Maybe (StgValue, StgValue))
readCons ms addr =
  peek addr >>= \case
    Closure {closureCode = lf, closureEnv = e} ->
      case lf of
        LambdaForm {_body = (App (Con t) xs)}
          | t == stgCons -> do
            h' <- val e ms (xs `Seq.index` 0)
            t' <- val e ms (xs `Seq.index` 1)
            return $ Just (h', t')
        LambdaForm {_body = (App (Con t) _)}
          | t == stgNil -> return Nothing
        _ -> throwIn ms $ IntrinsicExpectedEvaluatedList (_body lf)
    BlackHole -> throwIn ms IntrinsicExpectedListFoundBlackHole
    PartialApplication{} -> throwIn ms IntrinsicExpectedListFoundPartialApplication


-- | Assuming the specified address is a Block, return the contents list.
readBlock :: MachineState -> Address -> IO StgValue
readBlock ms addr =
  peek addr >>= \case
    Closure {closureCode = lf, closureEnv = e} ->
      case lf of
        LambdaForm {_body = (App (Con t) xs)}
          | t == stgBlock -> val e ms (xs `Seq.index` 0)
        LambdaForm {_body = (App (Con _) _)} ->
          throwIn ms $ IntrinsicExpectedBlock (_body lf)
        _ -> throwIn ms $ IntrinsicExpectedEvaluatedBlock (_body lf)
    BlackHole -> throwIn ms IntrinsicExpectedBlockFoundBlackHole
    PartialApplication {} ->
      throwIn ms IntrinsicExpectedBlockFoundPartialApplication

pattern Empty :: Seq a
pattern Empty <- (Seq.viewl -> Seq.EmptyL)

pattern (:<) :: a -> Seq a -> Seq a
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)

-- | class of Invokable intrinsic functions
class Invokable f where
  -- | String representation of expected signature for error messages etc.
  sig :: f -> String
  -- | Cast args as required to invoke the typed intrinsic function
  invoke :: f -> IntrinsicFunction

instance Invokable (MachineState -> Native -> IO MachineState) where
  sig _ = "*"
  invoke f ms (ValVec (StgNat a _ :< _)) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> String -> IO MachineState) where
  sig _ = "String"
  invoke f ms (ValVec (StgNat (NativeString a) _ :< _)) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Scientific -> IO MachineState) where
  sig _ = "Number"
  invoke f ms (ValVec (StgNat (NativeNumber a) _ :< _)) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> String -> String -> IO MachineState) where
  sig _ = "String, String"
  invoke f ms (ValVec (StgNat (NativeString a) _ :< (StgNat (NativeString b) _ :< _))) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Native -> String -> IO MachineState) where
  sig _ = "*, String"
  invoke f ms (ValVec (StgNat a _ :< (StgNat (NativeString b) _ :< _))) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Address -> String -> IO MachineState) where
  sig _ = "@, String"
  invoke f ms (ValVec (StgAddr a :< (StgNat (NativeString b) _ :< _))) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Scientific -> Scientific -> IO MachineState) where
  sig _ = "Number, Number"
  invoke f ms (ValVec (StgNat (NativeNumber a) _ :< (StgNat (NativeNumber b) _ :< _))) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

throwTypeError :: MachineState -> String -> ValVec -> IO MachineState
throwTypeError ms expected actual =
  throwIn ms $ IntrinsicTypeError expected $ describeActualArgs actual

describeActualArgs :: ValVec -> String
describeActualArgs (ValVec args) = intercalate ", " $ map describe $ toList args
  where
    describe (StgNat n _) = nativeToString n
    describe (StgAddr _) = "@"

nativeToString :: Native -> String
nativeToString n =
  case n of
    NativeNumber sc ->
      case floatingOrInteger sc of
        Left f -> show (f :: Double)
        Right i -> show (i :: Integer)
    NativeString s -> s
    NativeSymbol s -> s
    NativeBool b ->
      if b
        then "true"
        else "false"
    NativeSet _ -> "#SET"
    NativeDict _ -> "#DICT"
