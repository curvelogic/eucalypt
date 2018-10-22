{-|
Module      : Eucalypt.Stg.Globals.Bool
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
  ) where

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

euLt :: LambdaForm
euLt = binop "LT"

euGt :: LambdaForm
euGt = binop "GT"

euLte :: LambdaForm
euLte = binop "LTE"

euGte :: LambdaForm
euGte = binop "GTE"
