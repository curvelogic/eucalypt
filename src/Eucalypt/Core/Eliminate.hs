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
import qualified Data.Set as Set

-- | Eliminate unused bindings from let expressions.
--
-- TODO: this but efficiently... at present this is just to cut down
-- on noise while debugging.
prune :: CoreExp a -> CoreExp a
prune (CoreLet bs b) =
  let prunedB = pruneScope b
      usedInB = foldMapBound Set.singleton prunedB
      prunedBs = map (second pruneScope) bs
      usedInBs = mconcat $ map (foldMapBound Set.singleton . snd) prunedBs
      used = usedInB <> usedInBs
   in CoreLet (blankUnused used prunedBs) prunedB
  where
    pruneScope = toScope . prune . fromScope
    blankUnused u binds = map (blankNon u) (zip [0 ..] binds)
    blankNon u (i, el@(k, _)) =
      if i `Set.member` u
        then el
        else (k, toScope corenull)
prune (CoreBlock l) = CoreBlock $ prune l
prune (CoreOpSoup exprs) = CoreOpSoup $ map prune exprs
prune (CoreArgTuple exprs) = CoreArgTuple $ map prune exprs
prune (CoreList exprs) = CoreList $ map prune exprs
prune (CoreMeta m e) = CoreMeta (prune m) (prune e)
prune (CoreApply f exprs) = CoreApply (prune f) $ map prune exprs
prune (CoreLambda n body) = CoreLambda n $ (toScope . prune . fromScope) body
prune (CoreTraced expr) = CoreTraced $ prune expr
prune (CoreChecked t e) = CoreChecked (prune t) (prune e)
prune (CorePAp n f exprs) = CorePAp n (prune f) (map prune exprs)
prune e = e
