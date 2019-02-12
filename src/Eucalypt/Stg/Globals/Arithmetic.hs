{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Eucalypt.Stg.Globals.Arithmetic
Description : Arithmetic fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Arithmetic
  ( globals
  ) where

import Data.Symbol
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Globals.Common

globals :: [(Symbol, LambdaForm)]
globals =
  [ ("ADD", euAdd)
  , ("SUB", euSub)
  , ("MUL", euMul)
  , ("DIV", euDiv)
  , ("MOD", euMod)
  , ("FLOOR", euFloor)
  , ("CEILING", euCeiling)
  , ("LT", euLt)
  , ("GT", euGt)
  , ("LTE", euLte)
  , ("GTE", euGte)
  ]

euAdd :: LambdaForm
euAdd = wrapBifStrict "ADD"

euSub :: LambdaForm
euSub = wrapBifStrict "SUB"

euMul :: LambdaForm
euMul = wrapBifStrict "MUL"

euDiv :: LambdaForm
euDiv = wrapBifStrict "DIV"

euMod :: LambdaForm
euMod = wrapBifStrict "MOD"

euLt :: LambdaForm
euLt = wrapBifStrict "LT"

euGt :: LambdaForm
euGt = wrapBifStrict "GT"

euLte :: LambdaForm
euLte = wrapBifStrict "LTE"

euGte :: LambdaForm
euGte = wrapBifStrict "GTE"

euFloor :: LambdaForm
euFloor = wrapBifStrict "FLOOR"

euCeiling :: LambdaForm
euCeiling = wrapBifStrict "CEILING"
