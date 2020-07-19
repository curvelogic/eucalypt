{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Eucalypt.Stg.Intrinsics.Common
Description : Common utilities for intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Common
  ( returnDynamic
  , returnBool
  , returnNil
  , returnValue
  , returnList
  , allocFXs
  , readList
  , readNatList
  , readStrList
  , readPairList
  , cast
  , classify
  , classifyNative
  , Invokable(..)
  , IntrinsicFunction
  , module Eucalypt.Stg.Loaders
  , module Eucalypt.Stg.Scrapers
  ) where

import Data.Dynamic
import Data.Foldable (toList)
import Data.Scientific (Scientific)
import Data.Symbol
import Data.Typeable (Proxy(..), typeOf, typeRep)
import Eucalypt.Stg.Address (allocate, peek)
import Eucalypt.Stg.Error
import Eucalypt.Stg.Loaders
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Stg.Scrapers
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Type
import Eucalypt.Stg.Value

type IntrinsicFunction = MachineState -> ValVec -> IO MachineState

-- | Return any typeable instance wrapped in a dynamic native
returnDynamic :: Typeable a => MachineState -> a -> IO MachineState
returnDynamic ms a =
  return $ ms {machineCode = ReturnLit (NativeDynamic (toDyn a)) Nothing}

-- | Return a bool
returnBool :: MachineState -> Bool -> IO MachineState
returnBool ms b =
  return $ ms {machineCode = ReturnCon (boolTag b) mempty Nothing}

-- | Return an empty list
returnNil :: MachineState -> IO MachineState
returnNil ms = return $ ms {machineCode = ReturnCon stgNil mempty Nothing}

-- | Return a machine value
returnValue :: MachineState -> StgValue -> IO MachineState
returnValue ms v = return $ ms {machineCode = Eval (Atom $ L 0) (singleton v) }

-- | Allocate a closure which calls f with args xs
allocFXs :: MachineState -> Address -> ValVec -> IO Address
allocFXs ms f xs =
  allocate $
  Closure
    { closureCode =
        thunkn_ (argc + 1) (appfn_ (L 0) [L $ fromIntegral n | n <- [1 .. argc]])
    , closureEnv = singleton (StgAddr f) <> xs
    , closureCallStack = machineCallStack ms
    , closureMeta = MetadataPassThrough
    }
  where
    argc = envSize xs


-- | Utility to return a list from an intrinsic.
--
-- Allocates all links and then 'ReturnCon's back to caller.
returnList :: Loadable a => MachineState -> [a] -> IO MachineState
returnList ms [] = return $ setCode ms (ReturnCon stgNil mempty Nothing)
returnList ms (n:ns) = do
  h <- load ms n
  t <- load ms ns
  return $ setCode ms (ReturnCon stgCons (toVec [h, t]) Nothing)

-- | Utility to read a list from the machine into a native haskell
-- list for a primitive function.
readNatList :: MachineState -> Address -> IO [Native]
readNatList ms addr =
  scrape ms (StgAddr addr) >>= \case
    (Just ns) -> return ns
    Nothing ->
      throwIn ms $
      TypeMismatch
        { context = "Failed to read list of native values"
        , expected = [listType]
        , obtained = []
        , obtainedValues = [Just $ StgAddr addr]
        }

-- | Read a list of strings from machine into native haskell list
readStrList :: MachineState -> Address -> IO [String]
readStrList ms addr = readNatList ms addr >>= traverse convert
  where
    convert (NativeString s) = return s
    convert _ =
      throwIn ms $
      TypeMismatch
        { context = "Failed to read list of strings"
        , expected = [listType]
        , obtained = []
        , obtainedValues = [Just $ StgAddr addr]
        }

-- | Utility to read a list of pairs from the machine into a native
-- haskell list for an intrinsic function.
readPairList :: MachineState -> Address -> IO [(Symbol, StgValue)]
readPairList ms addr =
  scrape ms (StgAddr addr) >>= \case
    Just kvs -> return kvs
    Nothing -> return []

-- | class of Invokable intrinsic functions
class Invokable f where
  -- | String representation of expected signature for error messages etc.
  sig :: f -> [StgType]
  -- | Cast args as required to invoke the typed intrinsic function
  invoke :: f -> IntrinsicFunction

instance Invokable (MachineState -> IO MachineState) where
  sig _ = []
  invoke f ms (asSeq -> Empty) = f ms
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Native -> IO MachineState) where
  sig _ = [TypeNative]
  invoke f ms (asSeq -> (StgNat a _ :< _)) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Address -> IO MachineState) where
  sig _ = [TypeHeapObj]
  invoke f ms (asSeq -> (StgAddr a :< _)) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> StgValue -> IO MachineState) where
  sig _ = [TypeAny]
  invoke f ms (asSeq -> a :< _) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Dynamic -> IO MachineState) where
  sig _ = [TypeDynamic Nothing]
  invoke f ms (asSeq -> StgNat (NativeDynamic a) _ :< _) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> String -> IO MachineState) where
  sig _ = [TypeString]
  invoke f ms (asSeq -> StgNat (NativeString a) _ :< _) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Scientific -> IO MachineState) where
  sig _ = [TypeNumber]
  invoke f ms (asSeq -> StgNat (NativeNumber a) _ :< _) = f ms a
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> String -> String -> IO MachineState) where
  sig _ = [TypeString, TypeString]
  invoke f ms (asSeq -> (StgNat (NativeString a) _ :< (StgNat (NativeString b) _ :< _))) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Native -> Native -> IO MachineState) where
  sig _ = [TypeNative, TypeNative]
  invoke f ms (asSeq -> (StgNat a _ :< (StgNat b _ :< _))) = f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Address -> Address -> IO MachineState) where
  sig _ = [TypeHeapObj, TypeHeapObj]
  invoke f ms (asSeq -> (StgAddr a :< (StgAddr b :< _))) = f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Native -> String -> IO MachineState) where
  sig _ = [TypeNative, TypeString]
  invoke f ms (asSeq -> (StgNat a _ :< (StgNat (NativeString b) _ :< _))) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> StgValue -> StgValue -> IO MachineState) where
  sig _ = [TypeAny, TypeAny]
  invoke f ms (asSeq -> (a :< (b :< _))) = f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Dynamic -> Symbol -> IO MachineState) where
  sig _ = [TypeDynamic Nothing, TypeSymbol]
  invoke f ms (asSeq -> StgNat (NativeDynamic a) _ :< (StgNat (NativeSymbol b) _ :< _)) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Address -> String -> IO MachineState) where
  sig _ = [TypeHeapObj, TypeString]
  invoke f ms (asSeq -> StgAddr a :< (StgNat (NativeString b) _ :< _)) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Scientific -> Scientific -> IO MachineState) where
  sig _ = [TypeNumber, TypeNumber]
  invoke f ms (asSeq -> StgNat (NativeNumber a) _ :< (StgNat (NativeNumber b) _ :< _)) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Dynamic -> Dynamic -> IO MachineState) where
  sig _ = [TypeDynamic Nothing, TypeDynamic Nothing]
  invoke f ms (asSeq -> StgNat (NativeDynamic a) _ :< (StgNat (NativeDynamic b) _ :< _)) =
    f ms a b
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Native -> Native -> Native -> IO MachineState) where
  sig _ = [TypeNative, TypeNative, TypeNative]
  invoke f ms (asSeq -> StgNat a _ :< (StgNat b _ :< (StgNat c _ :< _))) =
    f ms a b c
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Dynamic -> Symbol -> StgValue -> IO MachineState) where
  sig _ = [TypeDynamic Nothing, TypeSymbol, TypeAny]
  invoke f ms (asSeq -> StgNat (NativeDynamic a) _ :< (StgNat (NativeSymbol b) _ :< (c :< _))) =
    f ms a b c
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Dynamic -> Dynamic -> Address -> IO MachineState) where
  sig _ = [TypeDynamic Nothing, TypeDynamic Nothing, TypeHeapObj]
  invoke f ms (asSeq -> StgNat (NativeDynamic a) _ :< (StgNat (NativeDynamic b) _ :< (StgAddr c :< _))) =
    f ms a b c
  invoke f ms args = throwTypeError ms (sig f) args

instance Invokable (MachineState -> Scientific -> Scientific -> Scientific -> Scientific -> Scientific -> Scientific -> String -> IO MachineState) where
  sig _ = [TypeNumber, TypeNumber, TypeNumber, TypeNumber, TypeNumber, TypeNumber, TypeString]
  invoke f ms (asSeq -> StgNat (NativeNumber a) _ :<
                       (StgNat (NativeNumber b) _ :<
                       (StgNat (NativeNumber c) _ :<
                       (StgNat (NativeNumber d) _ :<
                       (StgNat (NativeNumber e) _ :<
                       (StgNat (NativeNumber ff) _ :<
                       (StgNat (NativeString g) _ :< _))))))) =
    f ms a b c d e ff g
  invoke f ms args = throwTypeError ms (sig f) args

cast :: forall a . Typeable a => MachineState -> Dynamic -> IO a
cast ms dyn =
  case fromDynamic dyn of
    (Just x) -> return x
    Nothing ->
      throwIn ms $
      TypeMismatch
        { context = "Failed to extract value of required type"
        , expected = [TypeDynamic $ Just $ typeRep (Proxy :: Proxy a)]
        , obtained = [TypeDynamic $ Just $ dynTypeRep dyn]
        , obtainedValues = [Just $ StgNat (NativeDynamic dyn) Nothing]
        }

throwTypeError :: MachineState -> [StgType] -> ValVec -> IO MachineState
throwTypeError ms needed actual = do
  typeVec <- traverse classify actual
  throwIn ms $
    TypeMismatch
      { context = "Intrinsic received arguments of incorrect type"
      , expected = needed
      , obtained = toList typeVec
      , obtainedValues = map Just $ toList actual
      }

classifyNative :: Native -> StgType
classifyNative (NativeString _) = TypeString
classifyNative (NativeSymbol _) = TypeSymbol
classifyNative (NativeNumber _) = TypeNumber
classifyNative (NativeDynamic dyn) = TypeDynamic $ Just (typeOf dyn)


classify :: StgValue -> IO StgType
classify (StgNat n _) = return $ classifyNative n
classify (StgAddr a) = do
  obj <- peek a
  case obj of
    Closure {closureCode = LambdaForm {lamBody = App (Con t) _}} ->
      return $ TypeData [t]
    Closure {} -> return TypeClosure
    BlackHole -> return TypeBlackHole
    PartialApplication {} -> return TypePartialApplication
