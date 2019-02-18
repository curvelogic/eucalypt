{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Driver.Explain
Description : Command for explaining interpretation of CLI options (-n)
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Driver.Explain where

import Data.Maybe (fromMaybe)
import Eucalypt.Driver.Options
import Eucalypt.Syntax.Input
import Text.PrettyPrint

-- | Explain the command line options to stdout
explain :: EucalyptOptions -> IO ()
explain opts@EucalyptOptions {..} =
  putStrLn $ renderStyle (style {lineLength = 80}) doc
  where
    doc =
      vcat
        [ modeExplanation optionMode
        , sourceExplanation optionInputs optionLibPath
        , evaluandExplanation opts
        , formatExplanation $ fromMaybe "yaml" optionExportFormat
        ]

sentence :: String -> Doc
sentence = hsep . map text . words

para :: String -> Doc
para p = sentence p $$ text ""

title :: String -> Doc
title t = text bar $$ text ""
  where
    bar = "- " ++ t ++ " " ++ replicate (80 - length t - 3) '-'

modeExplanation :: CommandLineMode -> Doc
modeExplanation mode =
  vcat
    [ title ("Mode: " ++ modeName)
    , para $ "You are running eu in " ++ modeName ++ " mode."
    , modeDoc
    , para "Ergonomic is the default, use -B to engage batch mode instead."
    ]
  where
    modeName =
      case mode of
        Ergonomic -> "ergonomic"
        Batch -> "batch (non-ergonomic)"
    modeDoc =
      case mode of
        Ergonomic -> ergoDoc
        Batch -> batchDoc
    ergoDoc =
      para $
      "This engages several defaults to make interactive command line usage easier." ++
      "In particular, if you have a .eucalypt file in your home directory it will be imported automatically."
    batchDoc =
      para
        "This disables some defaulting useful for interactive use, ignores .eucalypt files and assumes a non-interactive terminal."



sourceExplanation :: [Input] -> [String] -> Doc
sourceExplanation inputs loadPath =
  vcat
    [ title "Inputs"
    , hang
        (para "The following sources of code and data are specified:")
        2
        (vcat (map inputDoc inputs)) $$
      text ""
    , para $
      "Some inputs are added automatically (Eufile if it exists, ~/.eucalypt in ergonomic mode, system defined inputs, prefixed with __). " ++
      "Input formats (eu, yaml, json, toml, text, csv) are inferred by default but can be specified explicitly using this @ syntax."
    , para
        "STDIN may be specified explicitly as \"-\" or detected and added implicitly."
    , hang
        (para
           "The following directories are on the library load path and will be searched for file dependencies:")
        2
        (vcat (map loadPathDoc loadPath)) $$
      text ""
    , para "Use -L to add an entry to the library load path."
    ]
  where
    inputDoc input = text "-" <+> text (show input)
    loadPathDoc path = text "-" <+> text path



evaluandExplanation :: EucalyptOptions -> Doc
evaluandExplanation EucalyptOptions {..} = vcat [title "Evaluand", doc, flagDoc]
  where
    doc =
      para $
      case optionEvaluand of
        Just e ->
          "You have specified an evaluand explicitly (" ++
          e ++ ") which will be evaluated and rendered."
        Nothing ->
          case optionTarget of
            Just t ->
              "You have specified a target explicitly (" ++
              t ++ ") which will be found, evaluated and rendered."
            Nothing ->
              "You have not specified an explicit evaluand or target. " ++
              "If a :main target exists in the inputs, it will be evaluated and rendered. " ++
              "Otherwise the entire body of the final input (" ++
              show (last optionInputs) ++ ") will be rendered."
    flagDoc = para "This can be controlled with -t and -e flags."



formatExplanation :: String -> Doc
formatExplanation format = vcat [title $ "Format: " ++ format, doc, flagDoc]
  where
    doc = para $ "Output will be rendered as " ++ fmt ++ "."
    flagDoc = para "This can be controlled using -x and -j flags."
    fmt =
      case format of
        "yaml" -> "yaml (the default)"
        s -> s
