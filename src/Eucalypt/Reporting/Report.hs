{-|
Module      : Eucalypt.Reporting.Report
Description : Facilities for reporting errors
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Reporting.Report where

import Eucalypt.Reporting.Error
import Eucalypt.Reporting.Classes
import System.Exit
import System.IO
import qualified Text.PrettyPrint as P
import Control.Exception.Safe

-- | Report any errors to stderr
reportErrors :: Show a => [a] -> IO ()
reportErrors = mapM_ (hPrint stderr)



-- | Report an error to the console
reportToConsole :: Reportable a => a -> IO ()
reportToConsole e = hPutStr stderr $ P.render $ report e



-- | Attempt an IO action, but report and abort in the case of a
-- reportable error.
tryOrReport :: IO a -> IO a
tryOrReport action = do
  result <- tryJust eucalyptError action
  case result of
    Left e -> reportToConsole e >> exitFailure
    Right v -> return v
  where
    eucalyptError :: EucalyptError -> Maybe EucalyptError
    eucalyptError = Just
