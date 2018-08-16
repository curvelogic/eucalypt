{-|
Module      : Eucalypt.Stg.Globals.Panic
Description : Various error builtins in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}


module Eucalypt.Stg.Globals.Panic
  ( euBomb
  , euPanic
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics

euBomb :: LambdaForm
euBomb = value_ $ appbif_ (intrinsicIndex "PANIC") [Literal $ NativeString "BOMB"]

euPanic :: LambdaForm
euPanic =
  lam_ 0 1 $
  force_ (Atom (BoundArg 0)) $ appbif_ (intrinsicIndex "PANIC") [Local 0]
