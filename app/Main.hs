module Main
where

import Eucalypt.Driver.Options
import Eucalypt.Driver.Evaluator (evaluate)
import Eucalypt.Core.EvalByName (whnfM)
import System.Exit


-- | Parse options and explain, evaluate or parse as appropriate
main :: IO ()
main = do
  options <- getOptions
  case command options of
    Explain -> print options >> exitSuccess
    _ -> evaluate options whnfM >>= exitWith
