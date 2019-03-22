{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.Time
Description : Time and date globlas
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Time
  ( globals
  ) where

import Data.Symbol
import Eucalypt.Stg.Globals.Common (wrapBifStrict)
import Eucalypt.Stg.GlobalInfo (gref)
import Eucalypt.Stg.Native (Native(..))
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.Tags

globals :: [(Symbol, LambdaForm)]
globals =
  [ ("IFIELDS", euIFields)
  , ("ZDT", euZdt)
  , ("ZDT.WRAP", euZdtWrap)
  , ("ZDT.UNWRAP", euZdtUnwrap)
  , ("ZDT.FIELDS", euZdtFields)
  ]

euIFields :: LambdaForm
euIFields =
  lam_ 0 1 $
  ann_ "__IFIELDS" 0 $
  force_ (Atom (L 0)) $
  appbif_ (intrinsicIndex "IFIELDS") [L 1]


-- | __ZDT(y, m, d, h, M, s, Z)
euZdt :: LambdaForm
euZdt = wrapBifStrict "ZDT"

-- | __ZDT.WRAP(zdt)
euZdtWrap :: LambdaForm
euZdtWrap = lam_ 0 1 $ ann_ "ZDT.WRAP" 0 $ appcon_ stgZDT [L 0]

-- | __ZDT.UNWRAP(zdt)
euZdtUnwrap :: LambdaForm
euZdtUnwrap =
  lam_ 0 1 $
  ann_ "ZDT.UNWRAP" 0 $
  casedef_ (Atom (L 0)) [(stgZDT, (1, atom_ (L 1)))] $
  appfn_ (gref "PANIC") [V $ NativeString "Expected zoned date time"]

-- | __ZDT.FIELDS(zdt)
euZdtFields :: LambdaForm
euZdtFields = wrapBifStrict "ZDT.FIELDS"
