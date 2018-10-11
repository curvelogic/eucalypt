{-|
Module      : Eucalypt.Syntax.Error
Description : Errors that occur while reading data
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Source.Error where

import Control.Exception.Safe (Exception)
import Eucalypt.Reporting.Common
import Eucalypt.Reporting.Classes
import Data.Typeable
import Data.Text (Text, unpack)
import Text.Libyaml (Event)

-- | The exceptions that can be thrown during parse of YAML / JSON
data DataParseException
  = UnexpectedEndOfEvents
  | UnexpectedEvent Event
  | FromYamlException Text
  | FromTomlException Text
  deriving (Typeable)

instance Show DataParseException where
  show UnexpectedEndOfEvents = "Reached end of data unexpectedly."
  show (UnexpectedEvent evt) = "Unexpected " ++ show evt ++ " event while ingesting data"
  show (FromYamlException ye) = "Error reading YAML: " ++ unpack ye
  show (FromTomlException te) = "Error reading TOML: " ++ unpack te

instance Exception DataParseException

instance Reportable DataParseException where
  code _ = Nothing
  report e = standardReport "DATA PARSE EXCEPTION" (show e)
