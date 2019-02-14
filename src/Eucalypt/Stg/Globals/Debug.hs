{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.Debug
Description : Utilities for troubleshooting
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Debug
  ( globals
  ) where

import Data.Symbol
import Eucalypt.Stg.Globals.Common
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

euSaturated :: LambdaForm
euSaturated = wrapBifStrict "SATURATED"

euConstructor :: LambdaForm
euConstructor =
  lam_ 0 1 $
  ann_ "__CONSTRUCTOR" 0 $
  casedef_
    (Atom $ L 0)
    [ (stgBlock, (0, Atom $ V $ NativeString "stgBlock"))
    , (stgCons, (0, Atom $ V $ NativeString "stgCons"))
    , (stgNil, (0, Atom $ V $ NativeString "stgNil"))
    , (stgUnit, (0, Atom $ V $ NativeString "stgUnit"))
    , (stgIOSMBlock, (0, Atom $ V $ NativeString "stgIOSMBlock"))
    ] $
  Atom $ V $ NativeString "(native)"

euDebugShow :: LambdaForm
euDebugShow = wrapBifStrict "DEBUGSHOW"

globals :: [(Symbol, LambdaForm)]
globals =
  [ ("SATURATED", euSaturated)
  , ("CONSTRUCTOR", euConstructor)
  , ("DEBUGSHOW", euDebugShow)
  ]
