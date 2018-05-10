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

import Control.Exception.Safe
import Eucalypt.Core.Pretty
import Eucalypt.Core.Syn

-- | All the errors that may occur during evaluation
data EvaluationError
  = MultipleErrors [EvaluationError]
  | ConcatArgumentNotList CoreExpr
  | ElementsArgumentNotBlock CoreExpr
  | LookupTargetNotList CoreExpr
  | LookupKeyNotStringLike CoreExpr
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
  | DivideByZero CoreExpr
  | NotList CoreExpr
  | Panic String CoreExpr
  | Bug String CoreExpr
  | EmptyList CoreExpr
  | NoSource
  deriving (Eq, Typeable)

instance Show EvaluationError where
  show (MultipleErrors es) = foldl1 (++) (map ((++ "\n") . show) es)
  show (ConcatArgumentNotList expr) = "Argument to concat not a list in " ++ pprint expr
  show (ElementsArgumentNotBlock expr) = "Argument to elements not a block in " ++ pprint expr
  show (LookupTargetNotList expr) = "Lookup target not a list in " ++ pprint expr
  show (LookupKeyNotStringLike expr) = "Lookup key not string-like in " ++ pprint expr
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
  show (DivideByZero expr) = "Division by zero, denominator " ++ pprint expr
  show (NotList expr) = "Expected list value, got " ++ pprint expr
  show (EmptyList expr) = "List was empty in " ++ pprint expr
  show (Panic message expr) = "Panic - " ++ message ++ " in " ++ pprint expr
  show (Bug message expr) = "BUG! " ++ message ++ " - " ++ pprint expr
  show NoSource = "No source"

instance Exception EvaluationError
