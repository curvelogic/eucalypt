{-|
Module      : Eucalypt.Core.Decatenater
Description : Pass for replacing catenations with applications
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Decatenater where

import Eucalypt.Core.Recursion
import Eucalypt.Core.Syn


-- | Replace __CAT with applications.
--
-- Later catenation may be overridable but for now this is
-- straightfoward and we can eliminate it entirely in core phase
-- before getting to the STG execution
decatenate :: CoreExp a -> CoreExp a
decatenate (CoreApply smid (CoreBuiltin _ "CAT") [x, f]) =
  CoreApply smid (decatenate f) [decatenate x]
decatenate e = walk decatenate e
