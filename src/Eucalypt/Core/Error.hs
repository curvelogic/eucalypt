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
  | LookupTargetNotList CoreExpr
  | LookupKeyNotStringLike CoreExpr
  | KeyNotFound String
  | BadBlockElement CoreExpr
  | BadBlockContent CoreExpr
  | NotWeakHeadNormalForm CoreExpr
  | UncallableExpression CoreExpr
  | BuiltinNotFound CoreBuiltinName CoreExpr
  | NotSupported String CoreExpr
  | Bug String CoreExpr
  | NoSource
  deriving (Eq, Typeable)

instance Show EvaluationError where
  show (MultipleErrors es) = foldl1 (++) (map ((++ "\n") . show) es)
  show (ConcatArgumentNotList expr) = "Argument to concat not a list: " ++ pprint expr
  show (LookupTargetNotList expr) = "Lookup target not a list: " ++ pprint expr
  show (LookupKeyNotStringLike expr) = "Lookup key not string-like: " ++ pprint expr
  show (KeyNotFound key) = "Key not found: " ++ key
  show (BadBlockElement expr) = "Bad block element: " ++ pprint expr
  show (BadBlockContent expr) = "Bad block content: " ++ pprint expr
  show (NotWeakHeadNormalForm expr) = "Expected weak head normal form: " ++ pprint expr
  show (UncallableExpression expr) = "Uncallable expression: " ++ pprint expr
  show (BuiltinNotFound name expr) = "No builtin \"" ++ name ++ "\": " ++ pprint expr
  show (NotSupported message expr) = "Not supported (yet) - " ++ message ++ pprint expr
  show (Bug message expr) = "BUG! " ++ message ++ " - " ++ pprint expr
  show NoSource = "No source"

instance Exception EvaluationError
