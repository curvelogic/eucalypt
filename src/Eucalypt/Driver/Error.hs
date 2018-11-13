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
  | UnsupportedURLScheme String -- ^ unsupported URL scheme
  | CyclicInputs [Input] -- ^ input imports form one or more cycles
  | UnknownResource String -- ^ resource by this name does not exist
  | MissingEvaluand -- ^ tried to load a non-existent CLI evaluand
  deriving (Typeable)

instance Show CommandError where
  show (InvalidInput i) = "Invalid input: " ++ show i
  show (UnsupportedURLScheme scheme) = "Unsupported URL scheme: " ++ show scheme
  show (CyclicInputs is) = "Cyclic imports! " ++ show is
  show (UnknownResource n) = "No such resource: " ++ show n
  show MissingEvaluand = "Tried to load a non-existent CLI evaluand"

badInputReport :: [Input] -> String -> P.Doc
badInputReport inputs msg =
  title "INPUT ERROR" P.$$ P.hang (P.text msg) 2 (P.vcat items)
  where
    listItem i = P.text "-" P.<+> P.text i
    items = map (listItem . show) inputs

messageReport :: String -> P.Doc
messageReport msg =
  title "INPUT ERROR" P.$$ P.text msg

instance Reportable CommandError where
  code _ = Nothing

  report (InvalidInput i) = badInputReport [i] "Could not understand input"
  report (UnsupportedURLScheme s) = messageReport ("Unsupported URL Scheme: " ++ s)
  report (CyclicInputs is) = badInputReport is "Found circular imports!"
  report (UnknownResource n) = messageReport ("No such resource: " ++ n)
  report MissingEvaluand = messageReport "Tried to load a non-existent CLI evaluand"

instance Exception CommandError
