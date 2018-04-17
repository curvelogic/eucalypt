module Eucalypt.Syntax.Error where

import Data.Typeable
import Text.Parsec.Error (ParseError)

newtype SyntaxError = SyntaxError ParseError
  deriving (Show, Eq, Typeable)
