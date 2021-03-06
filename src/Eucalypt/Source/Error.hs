{-|
Module      : Eucalypt.Syntax.Error
Description : Errors that occur while reading data
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Source.Error where

import Control.Exception.Safe
import Eucalypt.Reporting.Common
import Eucalypt.Reporting.Classes
import Eucalypt.Reporting.Location
import Data.Typeable ()
import Data.Text (Text, unpack)
import Text.Libyaml (Event)
import qualified Text.Megaparsec.Pos as M

-- | The exceptions that can be thrown during parse of YAML / JSON
data DataParseException
  = UnexpectedEndOfEvents
  | UnexpectedEvent Event
  | YamlError !String !String
  | YamlParseError !String !String !String !Int !Int
  | FromYamlException Text
  | FromTomlException Text
  | FromCsvException Text
  | FromXmlException
  deriving (Eq, Typeable)

instance Show DataParseException where
  show UnexpectedEndOfEvents = "Reached end of data unexpectedly."
  show (UnexpectedEvent evt) = "Unexpected " ++ show evt ++ " event while ingesting data"
  show (YamlError _loc msg) = msg
  show (YamlParseError msg ctxt locator _line _col)  = locator ++ ": " ++ msg ++ " " ++ ctxt
  show (FromYamlException ye) = "Error reading YAML: " ++ unpack ye
  show (FromTomlException te) = "Error reading TOML: " ++ unpack te
  show (FromCsvException te) = "Error reading CSV: " ++ unpack te
  show FromXmlException = "Error reading XML "

instance Exception DataParseException

instance Reportable DataParseException where
  code (YamlParseError _ _ locator line col) = Just (p, p)
    where
      p =
        SourcePosition
        M.SourcePos
          { M.sourceName = locator
          , M.sourceLine = M.mkPos line
          , M.sourceColumn = M.mkPos col
          }
  code _ = Nothing
  report e = standardReport "DATA PARSE EXCEPTION" (show e)
