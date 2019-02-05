{-|
Module      : Eucalypt.Stg.Globals.Panic
Description : Various error builtins in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}


module Eucalypt.Stg.Globals.Panic
  ( globals
  ) where

import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics

globals :: [(String, LambdaForm)]
globals = [("BOMB", euBomb), ("PANIC", euPanic), ("!KEYNOTFOUND", euKeyNotFound)]

euBomb :: LambdaForm
euBomb = value_ $ appbif_ (intrinsicIndex "PANIC") [V $ NativeString "BOMB"]

euPanic :: LambdaForm
euPanic =
  lam_ 0 1 $
  force_ (Atom (L 0)) $ appbif_ (intrinsicIndex "PANIC") [L 0]

euKeyNotFound :: LambdaForm
euKeyNotFound =
  lam_ 0 1 $
  force_ (appfn_ (gref "JOIN") [V $ NativeString "Key not found: ", L 0]) $
  appbif_ (intrinsicIndex "PANIC") [L 1]
