module Eucalypt.Syntax.Error where

import Control.Exception.Safe
import Data.Void
import qualified Text.Parsec.Error as P
import qualified Text.Megaparsec as M

data SyntaxError
  = SyntaxError P.ParseError
  | MegaparsecError (M.ParseError (M.Token String) Void)
  deriving (Show, Eq, Typeable)

instance Exception SyntaxError
