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

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

binop :: String -> LambdaForm
binop intrinsicName =
  lam_ 0 2 $
  ann_ ("__" ++ intrinsicName) 0 $
  force_ (Atom (L 0)) $
  force_ (Atom (L 1)) $
  appbif_ (intrinsicIndex intrinsicName) [L 2, L 3]

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
  force_ (Atom (L 0)) $ appbif_ (intrinsicIndex intrinsicName) [L 1]

euFloor :: LambdaForm
euFloor = unop "FLOOR"

euCeiling :: LambdaForm
euCeiling = unop "CEILING"

globals :: [(String, LambdaForm)]
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
