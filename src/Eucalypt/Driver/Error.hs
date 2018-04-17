module Eucalypt.Driver.Error where

import Data.Typeable
import Eucalypt.Driver.Input (Input)

data CommandError = InvalidInputMode Input
  deriving (Show, Typeable)
