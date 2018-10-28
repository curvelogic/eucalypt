{-|
Module      : Eucalypt.Core.Eliminate
Description : Pass for eliminating dead code
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Eliminate where


import Bound.Scope
import Data.Bifunctor
import Eucalypt.Core.Syn
import Eucalypt.Core.SourceMap
import qualified Data.Set as Set

-- | Eliminate unused bindings from let expressions.
--
-- TODO: this but efficiently... at present this is just to cut down
-- on noise while debugging.
prune :: CoreExp a -> CoreExp a
prune (CoreLet smid bs b) =
  let prunedB = pruneScope b
      usedInB = foldMapBound Set.singleton prunedB
      prunedBs = map (second pruneScope) bs
      usedInBs = mconcat $ map (foldMapBound Set.singleton . snd) prunedBs
      used = usedInB <> usedInBs
   in CoreLet smid (blankUnused used prunedBs) prunedB
  where
    pruneScope = toScope . prune . fromScope
    blankUnused u binds = map (blankNon u) (zip [0 ..] binds)
    blankNon u (i, el@(k, _)) =
      if i `Set.member` u
        then el
        else (k, toScope $ anon corenull)
prune (CoreBlock smid l) = CoreBlock smid $ prune l
prune (CoreOpSoup smid exprs) = CoreOpSoup smid $ map prune exprs
prune (CoreArgTuple smid exprs) = CoreArgTuple smid $ map prune exprs
prune (CoreList smid exprs) = CoreList smid $ map prune exprs
prune (CoreMeta smid m e) = CoreMeta smid (prune m) (prune e)
prune (CoreApply smid f exprs) = CoreApply smid (prune f) $ map prune exprs
prune (CoreLambda smid i n body) = CoreLambda smid i n $ (toScope . prune . fromScope) body
prune e = e
