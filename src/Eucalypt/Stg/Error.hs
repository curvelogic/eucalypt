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
import Eucalypt.Stg.Native
import Eucalypt.Stg.Pretty
import Eucalypt.Stg.Syn (Tag)
import Eucalypt.Stg.Type
import Eucalypt.Stg.Value
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
  = NoBranchFound
  | NoDefaultBranchForNativeReturn
  | EnteredBlackHole
  | AddMetaToBlackHole
  | ArgInsteadOfBranchTable
  | EnvironmentIndexOutOfRange !Int
  | SteppingTerminated
  | TypeMismatch { context :: !String
                 , expected :: ![StgType]
                 , obtained :: ![StgType]
                 , obtainedValues :: ![Maybe StgValue] }
  | BadConstructorArity !Tag
                        !Int
  | BadPair !StgValue
  | InvalidRegex !String
  | InvalidFormatSpecifier !String
                           !Native
  | InvalidArgument !String
  | UnknownGlobal !String
  | KeyNotFound !Native
  | Panic !String
  | IOSystem IOException
  | InvalidNumber !String
  | MissingArgument
  | AppliedDataStructureToMoreThanOneArgument
  deriving (Typeable, Show, Eq)

instance Exception StgException

execBug :: String -> P.Doc
execBug msg =
  standardReport "INTERNAL ERROR" msg P.$$ P.text "This is probably a bug in eu."

execError :: String -> P.Doc
execError = standardReport "EXECUTION ERROR"

ioSystemError :: String -> P.Doc
ioSystemError = standardReport "I/O ERROR"

formatTypeError :: String -> [StgType] -> [StgType] -> [Maybe StgValue] -> P.Doc
formatTypeError msg expected obtained vals =
  execError msg P.$$ P.text "Expected:" P.$$
  P.nest 4 (P.text (friendlySignature expected)) P.$$
  (P.text " Obtained:" P.$$ P.nest 4 vs <> P.text (friendlySignature obtained))
  where
    vs = P.hcat $ P.punctuate P.comma $ map (maybe P.empty prettify) vals


instance Reportable StgException where
  report StgException {..} =
    let bug = execBug
        err = execError
        sys = ioSystemError
     in case stgExcError of
          NoBranchFound -> bug "No branch available to handle value."
          NoDefaultBranchForNativeReturn ->
            err "No default branch to handle native return value"
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
          (InvalidRegex s) ->
            err "Regular expression was not valid:" P.$$
            P.nest 2 (P.text "-" P.<+> P.text s)
          (InvalidArgument s) ->
            err "Argument was not valid:" P.$$
            P.nest 2 (P.text "-" P.<+> P.text s)
          (InvalidFormatSpecifier s n) ->
            err ("Format specifier " ++ s ++ " invalid for value: ") P.$$
            prettify n
          (UnknownGlobal s) ->
            err "Unknown global:" P.$$ P.nest 2 (P.text "-" P.<+> P.text s)
          (KeyNotFound k) ->
            err "Key not found :" P.$$ P.nest 2 (P.text "-" P.<+> prettify k)
          (Panic s) -> err s
          (IOSystem e) -> sys $ show e
          (InvalidNumber n) ->
            err $ "Invalid number (" ++ show n ++ ") could not be parsed."
          MissingArgument -> bug "Expected argument but none found"
          AppliedDataStructureToMoreThanOneArgument ->
            bug "A data structure was applied to more than one argument"
          TypeMismatch {..} ->
            formatTypeError context expected obtained obtainedValues
          BadConstructorArity t n ->
            bug $
            "Constructor " ++
            show t ++ " has incorrect number of arguments (" ++ show n ++ ")"
          BadPair _ -> bug "Encounter bad pair"


instance HasSourceMapIds StgException where
  toSourceMapIds StgException{..} = toSourceMapIds stgExcCallStack
