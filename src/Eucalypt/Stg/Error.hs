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

data StgException
  = NonArgStackEntry
  | NonAddressStgValue
  | NonNativeStgValue
  | LiteralUpdate
  | NativeBranchTableForCon
  | ConBranchTableForNative
  | NoBranchFound
  | PopEmptyStack
  | EnteredBlackHole
  | ArgInsteadOfBranchTable
  | ArgInsteadOfNativeBranchTable
  | StackIndexOutOfRange
  | EnvironmentIndexOutOfRange
  | IntrinsicIndexOutOfRange
  | SteppingTerminated
  | AttemptToResolveBoundArg
  | IntrinsicExpectedNativeList
  | IntrinsicExpectedStringList
  | InvalidRegex !String
  deriving (Typeable, Show, Eq)

instance Exception StgException
