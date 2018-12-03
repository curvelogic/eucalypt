{-|
Module      : Eucalypt.Stg.Globals.Number
Description : Basic number fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Number
  ( euNumParse
  , globals
  ) where

import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

euNumParse :: LambdaForm
euNumParse =
  lam_ 0 1 $
  ann_ "__NUMPARSE" 0 $
  force_ (Atom (Local 0)) $ appbif_ (intrinsicIndex "NUMPARSE") [Local 1]

globals :: [GlobalInfo]
globals = [GlobalInfo "NUMPARSE" euNumParse [Strict]]
