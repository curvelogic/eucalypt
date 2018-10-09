{-|
Module      : Eucalypt.Reporting.Report
Description : Facilities for reporting errors
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Reporting.Report where

import Control.Exception.Safe
import qualified Data.ByteString as B
import qualified Eucalypt.Reporting.Code as Code
import Eucalypt.Reporting.Location
import Eucalypt.Reporting.Classes
import Eucalypt.Reporting.Error
import Eucalypt.Syntax.Input
import System.Exit
import System.IO
import qualified Text.PrettyPrint as P
import qualified Text.Megaparsec.Pos as M

-- | Report any errors to stderr
reportErrors :: Show a => [a] -> IO ()
reportErrors = mapM_ (hPrint stderr)



-- | Send a pretty print doc to stderr
consoleError :: P.Doc -> IO ()
consoleError = hPutStr stderr . P.render



-- | Report an error to the console
reportToConsole :: Reportable a => a -> IO ()
reportToConsole e = consoleError $ report e



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



-- | Attempt an IO action, but report and abort in the case of a
-- reportable error, using supplied means of resolving inputs to
-- bytestrings to format code for error reporting.
tryOrReportWithCode :: (Input -> IO B.ByteString) -> IO a -> IO a
tryOrReportWithCode resolve action = do
  result <- tryJust eucalyptError action
  case result of
    Left e -> do
      codeDoc <- case code e of
                   Nothing -> return P.empty
                   Just sp ->
                     case codeInput sp of
                       Nothing -> return $ P.text "!!! CANNOT FIND SOURCE !!!"
                       Just input -> (`codeToDoc` sp) <$> resolve input
      let messageDoc = report e
      consoleError (messageDoc P.$$ codeDoc <> P.char '\n') >> exitFailure
    Right v -> return v
  where
    eucalyptError :: EucalyptError -> Maybe EucalyptError
    eucalyptError = Just



-- | Parse the source location name as a eucalypt input
codeInput :: SourceSpan -> Maybe Input
codeInput (SourcePosition start, _) = parseInputFromString $ M.sourceName start



-- | Format code specified by bytestring and span
codeToDoc :: B.ByteString -> SourceSpan -> P.Doc
codeToDoc bytes (SourcePosition start, SourcePosition end) =
  Code.format bytes startLine startCol endLine endCol
  where
    startLine = M.unPos $ M.sourceLine start
    startCol = M.unPos $ M.sourceColumn start
    endLine = M.unPos $ M.sourceLine end
    endCol = M.unPos $ M.sourceColumn end
