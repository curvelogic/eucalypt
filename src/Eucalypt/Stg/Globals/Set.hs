{-|
Module      : Eucalypt.Stg.Globals.Set
Description : Set globals for STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Set
  ( globals
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [GlobalInfo]
globals =
  [ GlobalInfo "EMPTYSET" euEmptySet []
  , GlobalInfo "SETCONTAINS" euSetContains [Strict, Strict]
  , GlobalInfo "SETADD" euSetAdd [Strict, Strict]
  , GlobalInfo "SETREMOVE" euSetRemove [Strict, Strict]
  ]

euEmptySet :: LambdaForm
euEmptySet = thunk_ $ appbif_ (intrinsicIndex "EMPTYSET") []

euSetContains :: LambdaForm
euSetContains =
  lam_ 0 2 $
  ann_ "__SETCONTAINS" 0 $
  force_ (Atom $ Local 0) $
  force_ (Atom $ Local 1) $ appbif_ (intrinsicIndex "SETCONTAINS") [Local 2, Local 3]

euSetAdd :: LambdaForm
euSetAdd =
  lam_ 0 2 $
  ann_ "__SETADD" 0 $
  force_ (Atom $ Local 0) $
  force_ (Atom $ Local 1) $ appbif_ (intrinsicIndex "SETADD") [Local 2, Local 3]

euSetRemove :: LambdaForm
euSetRemove =
  lam_ 0 2 $
  ann_ "__SETREMOVE" 0 $
  force_ (Atom $ Local 0) $
  force_ (Atom $ Local 1) $
  appbif_ (intrinsicIndex "SETREMOVE") [Local 2, Local 3]
