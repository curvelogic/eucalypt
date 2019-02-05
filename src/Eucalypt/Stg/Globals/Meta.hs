{-|
Module      : Eucalypt.Stg.Globals.Meta
Description : Metadata fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Meta
  ( globals
  ) where

import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.Syn

globals :: [(String, LambdaForm)]
globals = [("META", euMeta), ("WITHMETA", euWithMeta)]

-- | __META(x)
euMeta :: LambdaForm
euMeta =
  lam_ 0 1 $
  ann_ "__META" 0 $
  force_ (Atom (L 0)) (appbif_ (intrinsicIndex "META") [L 1])


-- | __WITHMETA(m, o)
euWithMeta :: LambdaForm
euWithMeta =
  lam_ 0 2 $
  ann_ "__WITHMETA" 0 $
  appbif_ (intrinsicIndex "WITHMETA") [L 0, L 1]
