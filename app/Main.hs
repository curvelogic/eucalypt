module Main
where

import Eucalypt.Driver.Options
import Eucalypt.Driver.Evaluator (evaluate)
import Eucalypt.Core.EvalByName (whnfM)
import System.Exit
import Paths_eucalypt_hs (version)
import Data.Version (showVersion)

-- | Primary banner for version data
banner :: String
banner = "eu - Eucalypt (Haskell Impl: v" ++ showVersion version ++ ")"

-- | Parse options and explain, evaluate or parse as appropriate
main :: IO ()
main = do
  opts <- getOptions
  case optionCommand opts of
    Explain -> print opts >> exitSuccess
    ShowVersion -> putStrLn banner
    _ -> evaluate opts whnfM >>= exitWith
