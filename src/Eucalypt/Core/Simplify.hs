{-|
Module      : Eucalypt.Core.Simplify
Description : Passes for simplifying core code (including inlining
              decatenation, etc.)
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Simplify
  ( simplify
  ) where

import Control.Arrow ((>>>))
import Eucalypt.Core.Decatenater (decatenate)
import Eucalypt.Core.Inliner (inline)
import Eucalypt.Core.Eliminate (prune, compress)
import Eucalypt.Core.Verify (cleanEvaluand)
import Eucalypt.Core.Syn

-- | A suite of simplifying core passes, to be run after "cooking" but
-- prior to compilation to STG
simplify :: Show a => CoreExp a -> CoreExp a
simplify =
  let eliminateDeadCode = prune . prune . prune . prune
      runInlines = prune . prune . inline . inline . inline
   in eliminateDeadCode >>>
      runInlines >>> compress >>> decatenate >>> cleanEvaluand
