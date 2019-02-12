{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.Time
Description : Time and date globlas
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Time
  ( globals
  ) where

import Data.Symbol
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [(Symbol, LambdaForm)]
globals =
  [ ("IFIELDS", euIFields)
  ]

euIFields :: LambdaForm
euIFields =
  lam_ 0 1 $
  ann_ "__IFIELDS" 0 $
  force_ (Atom (L 0)) $
  appbif_ (intrinsicIndex "IFIELDS") [L 1]
