{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Eucalypt.Core.Error
Description : CoreError implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Error
  where

import Control.Exception.Safe
import Data.List (intercalate)
import Eucalypt.Core.Pretty
import Eucalypt.Core.SourceMap
import Eucalypt.Core.Syn
import Eucalypt.Reporting.Classes
import Eucalypt.Reporting.Common

data CoreExpShow = forall a. Show a => CoreExpShow (CoreExp a)

instance HasSourceMapIds CoreExpShow where
  toSourceMapIds (CoreExpShow a) = toSourceMapIds a

-- | All the errors that may occur during processing of core expressions
data CoreError
  = MultipleErrors [CoreError]
  | TooFewOperands CoreExpShow
  | InvalidOperatorOutputStack [CoreExpShow]
  | InvalidOperatorSequence CoreExpShow CoreExpShow
  | Bug String CoreExpShow
  | VerifyOperatorsFailed CoreExpShow
  | VerifyNamesFailed CoreExpShow
  | VerifyUnresolvedVar CoreExpShow
  | VerifyRedeclaration CoreExpShow
  | VerifyNoEliminated CoreExpShow
  | NoSource

instance Show CoreError where
  show (MultipleErrors es) = foldl1 (++) (map ((++ "\n") . show) es)
  show (TooFewOperands (CoreExpShow op)) =
    "Too few operands available for operator " ++ pprint op
  show (InvalidOperatorOutputStack exprs) =
    "Invalid output stack while cooking operator soup: [" ++
    intercalate "," (map (\(CoreExpShow s) -> pprint s) exprs) ++ "]"
  show (InvalidOperatorSequence (CoreExpShow l) (CoreExpShow r)) =
    "Invalid sequence of operators:" ++ pprint l ++ " " ++ pprint r
  show (VerifyOperatorsFailed (CoreExpShow expr)) =
    "Unresolved operator in " ++ pprint expr
  show (VerifyNoEliminated (CoreExpShow expr)) =
    "Eliminated code found " ++ pprint expr
  show (VerifyNamesFailed (CoreExpShow expr)) =
    "Found name nodes, not translated to vars:" ++ pprint expr
  show (VerifyUnresolvedVar (CoreExpShow (CoreUnresolved _ v))) =
    "Unresolved variable: " ++ v
  show (VerifyUnresolvedVar (CoreExpShow expr)) =
    "Unresolved variable: " ++ pprint expr
  show (VerifyRedeclaration (CoreExpShow (CoreRedeclaration _ v))) =
    "Redeclared variable: " ++ v
  show (VerifyRedeclaration (CoreExpShow expr)) =
    "Redeclared variable: " ++ pprint expr
  show (Bug message (CoreExpShow expr)) =
    "BUG! " ++ message ++ " - " ++ pprint expr
  show NoSource = "No source"

instance Exception CoreError

instance Reportable CoreError where
  report = standardReport "CORE ERROR" . show

instance HasSourceMapIds CoreError where
  toSourceMapIds (MultipleErrors es) = concatMap toSourceMapIds es
  toSourceMapIds (TooFewOperands op) = toSourceMapIds op
  toSourceMapIds (InvalidOperatorOutputStack exprs) = concatMap toSourceMapIds exprs
  toSourceMapIds (InvalidOperatorSequence l r) = concatMap toSourceMapIds [l, r]
  toSourceMapIds (VerifyOperatorsFailed expr) = toSourceMapIds expr
  toSourceMapIds (VerifyNamesFailed expr) = toSourceMapIds expr
  toSourceMapIds (VerifyUnresolvedVar expr) = toSourceMapIds expr
  toSourceMapIds (VerifyRedeclaration expr) = toSourceMapIds expr
  toSourceMapIds (Bug _ expr) = toSourceMapIds expr
  toSourceMapIds _ = []
