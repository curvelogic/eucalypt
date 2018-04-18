module Main
where

import Eucalypt.Driver.Options
import Eucalypt.Driver.Evaluator (evaluate)
import Eucalypt.Core.EvalByName (whnfM)
import System.Exit


-- | Parse options and explain, evaluate or parse as appropriate
main :: IO ()
main = do
  opts <- getOptions
  case optionCommand opts of
    Explain -> print opts >> exitSuccess
    _ -> evaluate opts whnfM >>= exitWith
