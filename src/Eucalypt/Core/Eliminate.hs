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
import Data.Maybe (fromMaybe)
import Eucalypt.Core.Syn
import qualified Data.Set as Set

-- | Eliminate unused bindings from let expressions.
--
-- TODO: this but efficiently... at present this is just to cut down
-- on noise while debugging and tidying up after inlining.
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
        else (k, toScope CoreEliminated)
prune (CoreBlock smid l) = CoreBlock smid $ prune l
prune (CoreOpSoup smid exprs) = CoreOpSoup smid $ map prune exprs
prune (CoreArgTuple smid exprs) = CoreArgTuple smid $ map prune exprs
prune (CoreList smid exprs) = CoreList smid $ map prune exprs
prune (CoreMeta smid m e) = CoreMeta smid (prune m) (prune e)
prune (CoreApply smid f exprs) = CoreApply smid (prune f) $ map prune exprs
prune (CoreLambda smid i n body) = CoreLambda smid i n $ (toScope . prune . fromScope) body
prune e = e



compress :: CoreExp a -> CoreExp a
compress (CoreLet smid bs b) =
  let compressedB = compressScope b
      compressedBs = map (second compressScope) bs
      indexRemapping = newBindIndexes $ map bindingIsNotEliminated compressedBs
      editedBindings =
        map (second $ remapBindings indexRemapping) $
        filter bindingIsNotEliminated compressedBs
   in CoreLet smid editedBindings $ remapBindings indexRemapping compressedB
  where
    compressScope = toScope . compress . fromScope
    bindingIsNotEliminated = not . isEliminated . unscope . snd
compress (CoreBlock smid l) = CoreBlock smid $ compress l
compress (CoreOpSoup smid exprs) = CoreOpSoup smid $ map compress exprs
compress (CoreArgTuple smid exprs) = CoreArgTuple smid $ map compress exprs
compress (CoreList smid exprs) = CoreList smid $ map compress exprs
compress (CoreMeta smid m e) = CoreMeta smid (compress m) (compress e)
compress (CoreApply smid f exprs) = CoreApply smid (compress f) $ map compress exprs
compress (CoreLambda smid i n body) = CoreLambda smid i n $ (toScope . compress . fromScope) body
compress e = e



remapBindings :: [Maybe Int] -> Scope Int CoreExp a -> Scope Int CoreExp a
remapBindings indexMapping = mapBound mapBinding
  where
    mapBinding :: Int -> Int
    mapBinding n = fromMaybe (- 1) (indexMapping !! n)



newBindIndexes :: [Bool] -> [Maybe Int]
newBindIndexes bs = result $ head $ filter done $ iterate step ([], 0, bs)
  where
    step (xs, n, True:rest) = (Just n : xs, n + 1, rest)
    step (xs, n, False:rest) = (Nothing : xs, n, rest)
    step r@(_, _, []) = r
    done (_, _, []) = True
    done _ = False
    result (r, _, _) = reverse r
