{-|
Module      : Eucalypt.Stg.Error
Description : Runtime errors from the STG machine
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Error where

import Control.Exception.Safe
import Eucalypt.Stg.CallStack

data StgException = StgException
  { stgExcError :: StgError
  , stgExcCallStack :: CallStack
  } deriving (Eq, Show, Typeable)

data StgError
  = NonArgStackEntry
  | NonAddressStgValue
  | NonNativeStgValue
  | LiteralUpdate
  | NoBranchFound
  | PopEmptyStack
  | EnteredBlackHole
  | ArgInsteadOfBranchTable
  | ArgInsteadOfNativeBranchTable
  | StackIndexOutOfRange
  | EnvironmentIndexOutOfRange !Int
  | IntrinsicIndexOutOfRange
  | SteppingTerminated
  | AttemptToResolveBoundArg
  | IntrinsicExpectedNativeList
  | IntrinsicExpectedStringList
  | InvalidRegex !String
  | UnknownGlobal !String
  | Panic !String
  | CompilerBug !String
  | IOSystem IOException
  deriving (Typeable, Show, Eq)

instance Exception StgException
