module Main
where

import Eucalypt.Driver.Options
import Eucalypt.Driver.Explain (explain)
import Eucalypt.Driver.Evaluator (evaluate)
import System.Exit
import Paths_eucalypt (version)
import Data.Version (showVersion)

-- | Primary banner for version data
banner :: String
banner = "eu - Eucalypt (Haskell Impl: v" ++ showVersion version ++ ")"

-- | Parse options and explain, evaluate or parse as appropriate
main :: IO ()
main = do
  opts <- getOptions
  case optionCommand opts of
    Explain -> explain opts >> exitSuccess
    ShowVersion -> putStrLn banner
    _ -> evaluate opts >>= exitWith
