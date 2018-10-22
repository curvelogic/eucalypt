{-|
Module      : Eucalypt.Core.Error
Description : Errors detected during driver stages
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Driver.Error where

import Control.Exception.Safe
import Eucalypt.Reporting.Common
import Eucalypt.Reporting.Classes
import Eucalypt.Syntax.Input (Input)
import qualified Text.PrettyPrint as P

data CommandError
  = InvalidInput Input -- ^ invalid input (bad format etc.)
  | CyclicInputs [Input] -- ^ input imports form one or more cycles
  deriving (Typeable)

instance Show CommandError where
  show (InvalidInput i) = "Invalid input: " ++ show i
  show (CyclicInputs is) = "Cyclic imports! " ++ show is

badInputReport :: [Input] -> String -> P.Doc
badInputReport inputs msg =
  title "INPUT ERROR" P.$$ P.hang (P.text msg) 2 (P.vcat items)
  where
    listItem i = P.text "-" P.<+> P.text i
    items = map (listItem . show) inputs

instance Reportable CommandError where
  code _ = Nothing

  report (InvalidInput i) = badInputReport [i] "Could not understand input"
  report (CyclicInputs is) = badInputReport is "Found circular imports!"

instance Exception CommandError
