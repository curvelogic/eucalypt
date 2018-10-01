{-|
Module      : Eucalypt.Syntax.Error
Description : Aggregated error type for all types of errors
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Reporting.Error
  where

import Control.Exception (SomeException)
import Control.Exception.Safe
import qualified Eucalypt.Core.Error as Core
import qualified Eucalypt.Driver.Error as Driver
import Eucalypt.Reporting.Classes
import qualified Eucalypt.Source.Error as Source
import qualified Eucalypt.Syntax.Error as Syntax
import qualified Text.PrettyPrint as P

-- | All the types of error that Eucalypt can experience and report
data EucalyptError
  = Core Core.CoreError
  | Source Source.DataParseException
  | Syntax Syntax.SyntaxError
  | System SomeException
  | Command Driver.CommandError
  deriving (Show, Typeable)

instance Exception EucalyptError

instance Reportable EucalyptError where
  code (Syntax e) = code e
  code _ = Nothing

  report (Syntax e) = report e
  report e = P.text $ show e
