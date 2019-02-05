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
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [(String, LambdaForm)]
globals =
  [ ("EMPTYSET", euEmptySet)
  , ("SETCONTAINS", euSetContains)
  , ("SETADD", euSetAdd)
  , ("SETREMOVE", euSetRemove)
  , ("SETMEMBERS", euSetMembers)
  ]

euEmptySet :: LambdaForm
euEmptySet = thunk_ $ appbif_ (intrinsicIndex "EMPTYSET") []

euSetContains :: LambdaForm
euSetContains =
  lam_ 0 2 $
  ann_ "__SETCONTAINS" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $ appbif_ (intrinsicIndex "SETCONTAINS") [L 2, L 3]

euSetAdd :: LambdaForm
euSetAdd =
  lam_ 0 2 $
  ann_ "__SETADD" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $ appbif_ (intrinsicIndex "SETADD") [L 2, L 3]

euSetRemove :: LambdaForm
euSetRemove =
  lam_ 0 2 $
  ann_ "__SETREMOVE" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $
  appbif_ (intrinsicIndex "SETREMOVE") [L 2, L 3]

euSetMembers :: LambdaForm
euSetMembers =
  lam_ 0 1 $
  ann_ "__SETMEMBERS" 0 $
  force_ (Atom $ L 0) $
  appbif_ (intrinsicIndex "SETMEMBERS") [L 1]
