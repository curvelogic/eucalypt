{-|
Module      : Eucalypt.Stg.Globals.List
Description : Standard list globals for the STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Globals.List
  ( euCons
  , euNil
  , euHead
  , euTail
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags


-- | __CONS(h, t)
euCons :: LambdaForm
euCons = standardConstructor 2 stgCons



-- | __NIL
euNil :: LambdaForm
euNil = standardConstructor 0 stgNil



-- | __HEAD(list)
euHead :: LambdaForm
euHead =
  lam_ 0 1 $
  case_ (Atom (BoundArg 0)) [(stgCons, (2, Atom (Local 1)))]



-- | __TAIL(list)
euTail :: LambdaForm
euTail =
  lam_ 0 1 $
  case_ (Atom (BoundArg 0)) [(stgCons, (2, Atom (Local 2)))]
