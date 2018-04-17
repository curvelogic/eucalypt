module Eucalypt.Reporting.Report where

import System.IO (hPrint, stderr)

-- | Report any errors to stderr
reportErrors :: Show a => [a] -> IO ()
reportErrors = mapM_ (hPrint stderr)
