{-|
Module      : Eucalypt.Stg.Globals.Dict
Description : Dict globals for STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Dict
  ( globals
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [(String, LambdaForm)]
globals =
  [ ("EMPTYDICT", euEmptyDict)
  , ("DICTCONTAINSKEY", euDictContainsKey)
  , ("DICTGET", euDictGet)
  , ("DICTPUT", euDictPut)
  , ("DICTDEL", euDictDel)
  , ("DICTENTRIES", euDictEntries)
  ]

euEmptyDict :: LambdaForm
euEmptyDict = thunk_ $ appbif_ (intrinsicIndex "EMPTYDICT") []

euDictContainsKey :: LambdaForm
euDictContainsKey =
  lam_ 0 2 $
  ann_ "__DICTCONTAINS" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $ appbif_ (intrinsicIndex "DICTCONTAINSKEY") [L 2, L 3]

euDictGet :: LambdaForm
euDictGet =
  lam_ 0 2 $
  ann_ "__DICTGET" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $ appbif_ (intrinsicIndex "DICTGET") [L 2, L 3]

euDictPut :: LambdaForm
euDictPut =
  lam_ 0 3 $
  ann_ "__DICTPUT" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $
  force_ (Atom $ L 2) $
  appbif_ (intrinsicIndex "DICTPUT") [L 3, L 4, L 5]

euDictDel :: LambdaForm
euDictDel =
  lam_ 0 2 $
  ann_ "__DICTDEL" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $
  appbif_ (intrinsicIndex "DICTDEL") [L 2, L 3]

euDictEntries :: LambdaForm
euDictEntries =
  lam_ 0 1 $
  ann_ "__DICTENTRIES" 0 $
  force_ (Atom $ L 0) $
  appbif_ (intrinsicIndex "DICTENTRIES") [L 1]
