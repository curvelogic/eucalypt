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

import Eucalypt.Stg.Syn
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [GlobalInfo]
globals =
  [ GlobalInfo "IFIELDS" euIFields [Strict]
  ]

euIFields :: LambdaForm
euIFields =
  lam_ 0 1 $
  ann_ "__IFIELDS" 0 $
  force_ (Atom (Local 0)) $
  appbif_ (intrinsicIndex "IFIELDS") [Local 1]
