{-|
Module      : Eucalypt.Stg.Globals.Meta
Description : Metadata fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Meta
  ( euMeta
  , euWithMeta
  ) where

import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.Syn


-- | __META(x)
euMeta :: LambdaForm
euMeta =
  lam_ 0 1 $
  ann_ "__META" $
  force_ (Atom (Local 0)) (appbif_ (intrinsicIndex "META") [Local 1])


-- | __WITHMETA(m, o)
euWithMeta :: LambdaForm
euWithMeta =
  lam_ 0 2 $
  ann_ "__WITHMETA" $
  appbif_ (intrinsicIndex "WITHMETA") [Local 0, Local 1]
