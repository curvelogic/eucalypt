module Eucalypt.Reporting.Error
  where

import qualified Eucalypt.Core.Error as Core
import qualified Eucalypt.Source.Error as Source
import qualified Eucalypt.Syntax.Error as Syntax
import qualified Eucalypt.Driver.Error as Driver
import Control.Exception (SomeException)


-- | All the types of error that Eucalypt can experience and report
data EucalyptError = Core Core.EvaluationError |
                     Source Source.DataParseException |
                     Syntax Syntax.SyntaxError |
                     System SomeException |
                     Command Driver.CommandError
  deriving (Show)
