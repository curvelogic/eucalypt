{-# LANGUAGE ExistentialQuantification #-}
{-|
Module      : Eucalypt.Core.Error
Description : EvaluationError implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Error
  where

import Data.List (intercalate)
import Control.Exception.Safe
import Eucalypt.Core.Pretty
import Eucalypt.Core.Syn

data CoreExpShow = forall a. Show a => CoreExpShow (CoreExp a)

-- | All the errors that may occur during evaluation
data EvaluationError
  = MultipleErrors [EvaluationError]
  | ConcatArgumentNotList CoreExpShow
  | ElementsArgumentNotBlock CoreExpShow
  | RemoveArgumentNotBlock CoreExpShow
  | TopLevelNotBlock CoreExpShow
  | LookupTargetNotList CoreExpShow
  | LookupKeyNotStringLike CoreExpShow
  | SymbolNamesMustBeStrings CoreExpShow
  | BadSplitArgs CoreExpShow CoreExpShow
  | BadMatchArgs CoreExpShow CoreExpShow
  | BadJoinArgs CoreExpShow CoreExpShow
  | KeyNotFound String
  | BadBlockElement CoreExpShow
  | BadBlockContent CoreExpShow
  | BadBlockMerge CoreExpShow
  | NotWeakHeadNormalForm CoreExpShow
  | UncallableExpression CoreExpShow
  | BuiltinNotFound CoreBuiltinName CoreExpShow
  | NotSupported String CoreExpShow
  | NotBoolean CoreExpShow
  | NotNumber CoreExpShow
  | NotString CoreExpShow
  | DivideByZero CoreExpShow
  | NotList CoreExpShow
  | TooFewOperands CoreExpShow
  | InvalidOperatorOutputStack [CoreExpShow]
  | InvalidOperatorSequence CoreExpShow CoreExpShow
  | InvalidArgConvention CoreExpShow
  | UnexpectedEndOfExpression CoreExpShow
  | Panic String CoreExpShow
  | Bug String CoreExpShow
  | EmptyList CoreExpShow
  | NoSource
  | AssertionFailed CoreExpShow

instance Show EvaluationError where
  show (MultipleErrors es) = foldl1 (++) (map ((++ "\n") . show) es)
  show (ConcatArgumentNotList (CoreExpShow expr)) = "Argument to concat not a list in " ++ pprint expr
  show (ElementsArgumentNotBlock (CoreExpShow expr)) = "Argument to elements not a block in " ++ pprint expr
  show (RemoveArgumentNotBlock (CoreExpShow expr)) = "Argument to remove not a block in " ++ pprint expr
  show (TopLevelNotBlock (CoreExpShow expr)) = "Top level must evaluate to block. Instead found: " ++ pprint expr
  show (LookupTargetNotList (CoreExpShow expr)) = "Lookup target not a list in " ++ show expr
  show (LookupKeyNotStringLike (CoreExpShow expr)) = "Lookup key not string-like in " ++ pprint expr
  show (SymbolNamesMustBeStrings (CoreExpShow expr)) = "Symbol name not string in " ++ pprint expr
  show (BadSplitArgs (CoreExpShow s) (CoreExpShow re)) = "Bad arguments to regex split - string:  " ++ pprint s ++ " regex: " ++ pprint re
  show (BadMatchArgs (CoreExpShow s) (CoreExpShow re)) = "Bad arguments to regex match - string:  " ++ pprint s ++ " regex: " ++ pprint re
  show (BadJoinArgs (CoreExpShow s) (CoreExpShow re)) = "Bad arguments to join - strings:  " ++ pprint s ++ " separator: " ++ pprint re
  show (KeyNotFound key) = "Key not found: " ++ key
  show (BadBlockElement (CoreExpShow expr)) = "Bad block element in " ++ pprint expr
  show (BadBlockContent (CoreExpShow expr)) = "Bad block content in " ++ pprint expr
  show (BadBlockMerge (CoreExpShow expr)) = "Block merge must take one block as argument. " ++ pprint expr
  show (NotWeakHeadNormalForm (CoreExpShow expr)) = "Expected weak head normal form in " ++ pprint expr
  show (UncallableExpression (CoreExpShow expr)) = "Uncallable expression in " ++ pprint expr
  show (BuiltinNotFound name (CoreExpShow expr)) = "Unknown builtin \"" ++ name ++ "\" referenced in " ++ pprint expr
  show (NotSupported message (CoreExpShow expr)) = "Not supported (yet) - " ++ message ++ pprint expr
  show (NotBoolean (CoreExpShow expr)) = "Expected boolean value, got " ++ pprint expr
  show (NotNumber (CoreExpShow expr)) = "Expected numeric values, got " ++ pprint expr
  show (NotString (CoreExpShow expr)) = "Expected string values got " ++ pprint expr
  show (DivideByZero (CoreExpShow expr)) = "Division by zero, denominator " ++ pprint expr
  show (NotList (CoreExpShow expr)) = "Expected list value, got " ++ pprint expr
  show (TooFewOperands (CoreExpShow op)) = "Too few operands available for operator " ++ pprint op
  show (InvalidOperatorOutputStack exprs) = "Invalid output stack while cooking operator soup: [" ++ intercalate "," (map (\(CoreExpShow s) -> pprint s) exprs) ++ "]"
  show (InvalidOperatorSequence (CoreExpShow l) (CoreExpShow r)) = "Invalid sequence of operators:" ++ pprint l ++ " " ++ pprint r
  show (InvalidArgConvention (CoreExpShow exprs)) = "Args passed incorrectly. " ++ pprint exprs
  show (UnexpectedEndOfExpression (CoreExpShow expr)) = "UnexpectedEndOfExpression after " ++ pprint expr
  show (EmptyList (CoreExpShow expr)) = "List was empty in " ++ pprint expr
  show (Panic message (CoreExpShow expr)) = "Panic - " ++ message ++ " in " ++ pprint expr
  show (Bug message (CoreExpShow expr)) = "BUG! " ++ message ++ " - " ++ pprint expr
  show (AssertionFailed (CoreExpShow expr)) = "Assertion Failed. " ++ pprint expr
  show NoSource = "No source"

instance Exception EvaluationError
