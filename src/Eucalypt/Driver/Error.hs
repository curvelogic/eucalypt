module Eucalypt.Driver.Error where

import Data.Typeable
import Eucalypt.Syntax.Input (Input)

newtype CommandError = InvalidInput Input
  deriving (Show, Typeable)
