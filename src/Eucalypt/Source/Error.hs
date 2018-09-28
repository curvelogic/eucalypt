module Eucalypt.Source.Error where

import Control.Exception.Safe (Exception)
import Data.Typeable
import Data.Text (Text)
import Text.Libyaml (Event)

-- | The exceptions that can be thrown during parse of YAML / JSON
data DataParseException
  = UnexpectedEndOfEvents
  | UnexpectedEvent Event
  | FromYamlException Text
  | FromTomlException Text
  deriving (Show, Typeable)

instance Exception DataParseException
