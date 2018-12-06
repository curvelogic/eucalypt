{-|
Module      : Eucalypt.Stg.Globals.Arithmetic
Description : Arithmetic fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Arithmetic
  ( euAdd
  , euSub
  , euMul
  , euDiv
  , euLt
  , euLte
  , euGt
  , euGte
  , globals
  ) where

import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

binop :: String -> LambdaForm
binop intrinsicName =
  lam_ 0 2 $
  ann_ ("__" ++ intrinsicName) 0 $
  force_ (Atom (Local 0)) $
  force_ (Atom (Local 1)) $
  appbif_ (intrinsicIndex intrinsicName) [Local 2, Local 3]

euAdd :: LambdaForm
euAdd = binop "ADD"

euSub :: LambdaForm
euSub = binop "SUB"

euMul :: LambdaForm
euMul = binop "MUL"

euDiv :: LambdaForm
euDiv = binop "DIV"

euMod :: LambdaForm
euMod = binop "MOD"

euLt :: LambdaForm
euLt = binop "LT"

euGt :: LambdaForm
euGt = binop "GT"

euLte :: LambdaForm
euLte = binop "LTE"

euGte :: LambdaForm
euGte = binop "GTE"

unop :: String -> LambdaForm
unop intrinsicName =
  lam_ 0 1 $
  ann_ ("__" ++ intrinsicName) 0 $
  force_ (Atom (Local 0)) $ appbif_ (intrinsicIndex intrinsicName) [Local 1]

euFloor :: LambdaForm
euFloor = unop "FLOOR"

euCeiling :: LambdaForm
euCeiling = unop "CEILING"

globals :: [GlobalInfo]
globals =
  [ GlobalInfo "ADD" euAdd [Strict, Strict]
  , GlobalInfo "SUB" euSub [Strict, Strict]
  , GlobalInfo "MUL" euMul [Strict, Strict]
  , GlobalInfo "DIV" euDiv [Strict, Strict]
  , GlobalInfo "MOD" euMod [Strict, Strict]
  , GlobalInfo "FLOOR" euFloor [Strict]
  , GlobalInfo "CEILING" euCeiling [Strict]
  , GlobalInfo "LT" euLt [Strict, Strict]
  , GlobalInfo "GT" euGt [Strict, Strict]
  , GlobalInfo "LTE" euLte [Strict, Strict]
  , GlobalInfo "GTE" euGte [Strict, Strict]
  ]
