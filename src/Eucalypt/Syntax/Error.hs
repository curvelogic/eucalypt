module Eucalypt.Syntax.Error where

import Control.Exception.Safe
import Data.Void
import qualified Text.Megaparsec as M

newtype SyntaxError
  = MegaparsecError (M.ParseError (M.Token String) Void)
  deriving (Show, Eq, Typeable)

instance Exception SyntaxError
