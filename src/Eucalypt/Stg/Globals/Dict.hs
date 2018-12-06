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
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [GlobalInfo]
globals =
  [ GlobalInfo "EMPTYDICT" euEmptyDict []
  , GlobalInfo "DICTCONTAINSKEY" euDictContainsKey [Strict, Strict]
  , GlobalInfo "DICTGET" euDictGet [Strict, Strict]
  , GlobalInfo "DICTPUT" euDictPut [Strict, Strict, Strict]
  , GlobalInfo "DICTDEL" euDictDel [Strict, Strict]
  , GlobalInfo "DICTENTRIES" euDictEntries [Strict]
  ]

euEmptyDict :: LambdaForm
euEmptyDict = thunk_ $ appbif_ (intrinsicIndex "EMPTYDICT") []

euDictContainsKey :: LambdaForm
euDictContainsKey =
  lam_ 0 2 $
  ann_ "__DICTCONTAINS" 0 $
  force_ (Atom $ Local 0) $
  force_ (Atom $ Local 1) $ appbif_ (intrinsicIndex "DICTCONTAINSKEY") [Local 2, Local 3]

euDictGet :: LambdaForm
euDictGet =
  lam_ 0 2 $
  ann_ "__DICTGET" 0 $
  force_ (Atom $ Local 0) $
  force_ (Atom $ Local 1) $ appbif_ (intrinsicIndex "DICTGET") [Local 2, Local 3]

euDictPut :: LambdaForm
euDictPut =
  lam_ 0 3 $
  ann_ "__DICTPUT" 0 $
  force_ (Atom $ Local 0) $
  force_ (Atom $ Local 1) $
  force_ (Atom $ Local 2) $
  appbif_ (intrinsicIndex "DICTPUT") [Local 3, Local 4, Local 5]

euDictDel :: LambdaForm
euDictDel =
  lam_ 0 2 $
  ann_ "__DICTDEL" 0 $
  force_ (Atom $ Local 0) $
  force_ (Atom $ Local 1) $
  appbif_ (intrinsicIndex "DICTDEL") [Local 2, Local 3]

euDictEntries :: LambdaForm
euDictEntries =
  lam_ 0 1 $
  ann_ "__DICTENTRIES" 0 $
  force_ (Atom $ Local 0) $
  appbif_ (intrinsicIndex "DICTENTRIES") [Local 1]
