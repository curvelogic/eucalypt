module Main
where

import Eucalypt.Driver.Options
import Eucalypt.Driver.Evaluator (evaluate)

-- | For now, just explain
main :: IO ()
main = do
  options <- getOptions
  case command options of
    Explain -> print options
    Evaluate -> evaluate options
    Parse -> evaluate options -- handles the parse case
