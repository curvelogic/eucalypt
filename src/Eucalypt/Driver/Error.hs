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
  = InvalidInput Input          -- ^ invalid input (bad format etc.)
  | InvalidImport String        -- ^ import somehow otherwise invalide
  | UnsupportedURLScheme String -- ^ unsupported URL scheme
  | CyclicInputs [Input]        -- ^ input imports form one or more cycles
  | UnknownResource String      -- ^ resource by this name does not exist
  | MissingEvaluand             -- ^ tried to load a non-existent CLI evaluand
  | CouldNotLoadFile String
                     [String]   -- ^ could not load file from load path
  | BadCacheDir FilePath
                String          -- ^ problem with the git repo cache
  deriving (Typeable)

instance Show CommandError where
  show (InvalidInput i) = "Invalid input: " ++ show i
  show (InvalidImport msg) = "Invalid import: " ++ msg
  show (UnsupportedURLScheme scheme) = "Unsupported URL scheme: " ++ show scheme
  show (CyclicInputs is) = "Cyclic imports! " ++ show is
  show (UnknownResource n) = "No such resource: " ++ show n
  show MissingEvaluand = "Tried to load a non-existent CLI evaluand"
  show (CouldNotLoadFile file libPath) =
    "Could not load file " ++ file ++ "from load path: " ++ show libPath
  show (BadCacheDir dir msg) =
    "Eucalypt import cache (" ++ dir ++ ") is invalid: " ++ msg

badInputReport :: [Input] -> String -> P.Doc
badInputReport inputs msg =
  title "INPUT ERROR" P.$$ P.hang (P.text msg) 2 (P.vcat items)
  where
    listItem i = P.text "-" P.<+> P.text i
    items = map (listItem . show) inputs

messageReport :: String -> P.Doc
messageReport msg =
  title "INPUT ERROR" P.$$ P.text msg

fancyReport :: P.Doc -> P.Doc
fancyReport doc = title "INPUT ERROR" P.$$ doc


instance Reportable CommandError where
  code _ = Nothing
  report (InvalidInput i) = badInputReport [i] "Could not understand input"
  report (InvalidImport msg) = messageReport ("Invalid import: " ++ msg)
  report (UnsupportedURLScheme s) =
    messageReport ("Unsupported URL Scheme: " ++ s)
  report (CyclicInputs is) = badInputReport is "Found circular imports!"
  report (UnknownResource n) = messageReport ("No such resource: " ++ n)
  report MissingEvaluand =
    messageReport "Tried to load a non-existent CLI evaluand"
  report (CouldNotLoadFile file libPath) =
    fancyReport $
    P.hang (P.text $ "Failed to load " ++ file ++ " from load path:") 2 $
    P.vcat $ map ((P.text "-" P.<+>) . P.text) libPath
  report (BadCacheDir dir msg) =
    messageReport "Eucalypt import cache is invalid." P.$$ P.text "" P.$$
    (P.text "The cache is at:" P.$$ P.nest 2 (P.text dir)) P.$$
    P.text "" P.$$
    P.text msg P.$$
    P.text "" P.$$
    P.text "Try deleting the cache."

instance Exception CommandError
