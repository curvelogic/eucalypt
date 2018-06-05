module Eucalypt.Syntax.Error where

import Data.Void
import Data.Typeable
import qualified Text.Parsec.Error as P
import qualified Text.Megaparsec as M

data SyntaxError
  = SyntaxError P.ParseError
  | MegaparsecError (M.ParseError (M.Token String) Void)
  deriving (Show, Eq, Typeable)
