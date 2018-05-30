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

-- | All the errors that may occur during evaluation
data EvaluationError
  = MultipleErrors [EvaluationError]
  | ConcatArgumentNotList CoreExpr
  | ElementsArgumentNotBlock CoreExpr
  | RemoveArgumentNotBlock CoreExpr
  | TopLevelNotBlock CoreExpr
  | LookupTargetNotList CoreExpr
  | LookupKeyNotStringLike CoreExpr
  | SymbolNamesMustBeStrings CoreExpr
  | BadSplitArgs CoreExpr CoreExpr
  | BadMatchArgs CoreExpr CoreExpr
  | BadJoinArgs CoreExpr CoreExpr
  | KeyNotFound String
  | BadBlockElement CoreExpr
  | BadBlockContent CoreExpr
  | BadBlockMerge CoreExpr
  | NotWeakHeadNormalForm CoreExpr
  | UncallableExpression CoreExpr
  | BuiltinNotFound CoreBuiltinName CoreExpr
  | NotSupported String CoreExpr
  | NotBoolean CoreExpr
  | NotNumber CoreExpr
  | NotString CoreExpr
  | DivideByZero CoreExpr
  | NotList CoreExpr
  | TooFewOperands CoreExpr
  | InvalidOperatorOutputStack [CoreExpr]
  | InvalidOperatorSequence CoreExpr CoreExpr
  | UnexpectedEndOfExpression CoreExpr
  | Panic String CoreExpr
  | Bug String CoreExpr
  | EmptyList CoreExpr
  | NoSource
  | AssertionFailed CoreExpr
  deriving (Eq, Typeable)

instance Show EvaluationError where
  show (MultipleErrors es) = foldl1 (++) (map ((++ "\n") . show) es)
  show (ConcatArgumentNotList expr) = "Argument to concat not a list in " ++ pprint expr
  show (ElementsArgumentNotBlock expr) = "Argument to elements not a block in " ++ pprint expr
  show (RemoveArgumentNotBlock expr) = "Argument to remove not a block in " ++ pprint expr
  show (TopLevelNotBlock expr) = "Top level must evaluate to block. Instead found: " ++ pprint expr
  show (LookupTargetNotList expr) = "Lookup target not a list in " ++ show expr
  show (LookupKeyNotStringLike expr) = "Lookup key not string-like in " ++ pprint expr
  show (SymbolNamesMustBeStrings expr) = "Symbol name not string in " ++ pprint expr
  show (BadSplitArgs s re) = "Bad arguments to regex split - string:  " ++ pprint s ++ " regex: " ++ pprint re
  show (BadMatchArgs s re) = "Bad arguments to regex match - string:  " ++ pprint s ++ " regex: " ++ pprint re
  show (BadJoinArgs s re) = "Bad arguments to join - strings:  " ++ pprint s ++ " separator: " ++ pprint re
  show (KeyNotFound key) = "Key not found: " ++ key
  show (BadBlockElement expr) = "Bad block element in " ++ pprint expr
  show (BadBlockContent expr) = "Bad block content in " ++ pprint expr
  show (BadBlockMerge expr) = "Block merge must take one block as argument. " ++ pprint expr
  show (NotWeakHeadNormalForm expr) = "Expected weak head normal form in " ++ pprint expr
  show (UncallableExpression expr) = "Uncallable expression in " ++ pprint expr
  show (BuiltinNotFound name expr) = "Unknown builtin \"" ++ name ++ "\" referenced in " ++ pprint expr
  show (NotSupported message expr) = "Not supported (yet) - " ++ message ++ pprint expr
  show (NotBoolean expr) = "Expected boolean value, got " ++ pprint expr
  show (NotNumber expr) = "Expected numeric values, got " ++ pprint expr
  show (NotString expr) = "Expected string values got " ++ pprint expr
  show (DivideByZero expr) = "Division by zero, denominator " ++ pprint expr
  show (NotList expr) = "Expected list value, got " ++ pprint expr
  show (TooFewOperands op) = "Too few operands available for operator " ++ pprint op
  show (InvalidOperatorOutputStack exprs) = "Invalid output stack while cooking operator soup: [" ++ intercalate ","(map pprint exprs) ++ "]"
  show (InvalidOperatorSequence l r) = "Invalid sequence of operators:" ++ pprint l ++ " " ++ pprint r
  show (UnexpectedEndOfExpression expr) = "UnexpectedEndOfExpression after " ++ pprint expr
  show (EmptyList expr) = "List was empty in " ++ pprint expr
  show (Panic message expr) = "Panic - " ++ message ++ " in " ++ pprint expr
  show (Bug message expr) = "BUG! " ++ message ++ " - " ++ pprint expr
  show (AssertionFailed expr) = "Assertion Failed. " ++ pprint expr
  show NoSource = "No source"

instance Exception EvaluationError
