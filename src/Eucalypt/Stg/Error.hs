{-# LANGUAGE RecordWildCards #-}
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
import Eucalypt.Core.SourceMap
import Eucalypt.Stg.CallStack
import Eucalypt.Stg.Syn
import Eucalypt.Reporting.Common
import Eucalypt.Reporting.Classes
import qualified Text.PrettyPrint as P

-- | Error during exception of STG machine
data StgException = StgException
  { stgExcError :: StgError
  , stgExcCallStack :: CallStack
  } deriving (Eq, Show, Typeable)

-- | Types of execution error
data StgError
  = NonAddressStgValue
  | NonNativeStgValue
  | NoBranchFound
  | EnteredBlackHole
  | AddMetaToBlackHole
  | ArgInsteadOfBranchTable
  | EnvironmentIndexOutOfRange !Int
  | SteppingTerminated
  | IntrinsicImproperList
  | IntrinsicBadPair
  | IntrinsicExpectedListFoundNative !Native
  | IntrinsicExpectedListFoundBlackHole
  | IntrinsicExpectedListFoundPartialApplication
  | IntrinsicExpectedNativeList
  | IntrinsicExpectedStringList
  | IntrinsicExpectedEvaluatedList !StgSyn
  | IntrinsicExpectedBlockFoundBlackHole
  | IntrinsicExpectedBlockFoundPartialApplication
  | IntrinsicExpectedBlockFoundNative !Native
  | IntrinsicExpectedEvaluatedBlock !StgSyn
  | IntrinsicExpectedBlock !StgSyn
  | InvalidRegex !String
  | UnknownGlobal !String
  | DictKeyNotFound !Native
  | Panic !String
  | IOSystem IOException
  | InvalidNumber !String
  | MissingArgument
  deriving (Typeable, Show, Eq)

instance Exception StgException

execBug :: String -> P.Doc
execBug msg =
  standardReport "INTERNAL ERROR" msg P.$$ P.text "This is probably a bug in eu."

execError :: String -> P.Doc
execError = standardReport "EXECUTION ERROR"

ioSystemError :: String -> P.Doc
ioSystemError = standardReport "I/O ERROR"

instance Reportable StgException where
  report StgException {..} =
    let bug = execBug
        err = execError
        sys = ioSystemError
     in case stgExcError of
          NonAddressStgValue ->
            bug "Found a native value when expecting a thunk."
          NonNativeStgValue ->
            err "A native value is expected here."
          NoBranchFound -> bug "No branch available to handle value."
          EnteredBlackHole ->
            err "Entered a black hole. This may indicate a circular definition."
          AddMetaToBlackHole -> bug "Attempted to add metadata to a black hole."
          ArgInsteadOfBranchTable ->
            bug "Need a branch table but found an apply argument continuation."
          (EnvironmentIndexOutOfRange i) ->
            bug $
            "Index into local environment (" ++ show i ++ ") is out of range."
          SteppingTerminated ->
            bug "Attempted to run a machine that had already terminated."
          IntrinsicImproperList -> err "Improper list found in block."
          IntrinsicBadPair -> err "Bad pair found in list"
          IntrinsicExpectedListFoundNative n ->
            err "Expected a list but found native value: " P.$$ prettify n
          IntrinsicExpectedListFoundBlackHole ->
            bug "Expected a list, found a black hole."
          IntrinsicExpectedListFoundPartialApplication ->
            bug "Expected a list, found a partial application."
          IntrinsicExpectedBlockFoundBlackHole ->
            bug "Expected a block, found a black hole."
          IntrinsicExpectedBlockFoundPartialApplication ->
            bug "Expected a block, found a partial application."
          IntrinsicExpectedNativeList -> err "Expected list of native values."
          IntrinsicExpectedStringList -> err "Expected list of strings."
          IntrinsicExpectedEvaluatedList expr ->
            bug "Expected evaluated list, found unevaluated thunks." P.$$
            prettify expr
          IntrinsicExpectedBlock expr ->
            bug "Expected block, found something else." P.$$
            prettify expr
          IntrinsicExpectedEvaluatedBlock expr ->
            bug "Expected evaluated block, found unevaluated thunks." P.$$
            prettify expr
          IntrinsicExpectedBlockFoundNative n ->
            err "Expected a block but found native value: " P.$$ prettify n
          (InvalidRegex s) ->
            err "Regular expression was not valid:" P.$$
            P.nest 2 (P.text "-" P.<+> P.text s)
          (UnknownGlobal s) ->
            err "Unknown global:" P.$$ P.nest 2 (P.text "-" P.<+> P.text s)
          (DictKeyNotFound k) ->
            err "Dict key not found :" P.$$ P.nest 2 (P.text "-" P.<+> prettify k)
          (Panic s) -> err s
          (IOSystem e) -> sys $ show e
          (InvalidNumber n) -> err $ "Invalid number (" ++ show n ++ ") could not be parsed."
          MissingArgument -> bug "Expected argument but none found"


instance HasSourceMapIds StgException where
  toSourceMapIds StgException{..} = toSourceMapIds stgExcCallStack
