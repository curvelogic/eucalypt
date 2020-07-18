{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Eucalypt.Core.BlockAnaphora
Description : Process block anaphora to turn anaphoric blocks to lambdas
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.BlockAnaphora
  ( anaphorise
  , hasNakedBlockAnaphora
  ) where

import Bound
import Data.Bifunctor (second)
import Eucalypt.Core.Anaphora
import Eucalypt.Core.Syn


-- | Process block expressions containing block anaphora into lambdas.
--
-- This processing assumes that all CoreLets represent blocks and so
-- should be applied early in the pipeline - directly after desugaring
-- so that it is not disrupted by any transformations or optimisations
-- which disturb lets.
anaphorise :: (Anaphora SymbolicAnaphora a) => CoreExp a -> CoreExp a
anaphorise = transform False



-- | Search from this level to see if we have any block anaphora which
-- are not contained within a block expression.
hasNakedBlockAnaphora :: (Anaphora SymbolicAnaphora a) => CoreExp a -> Bool
hasNakedBlockAnaphora (CoreVar _ n) = isAnaphor blockAnaphora n
hasNakedBlockAnaphora CoreLet{} = False
hasNakedBlockAnaphora (CoreLambda _ _ _ b) =
  (hasNakedBlockAnaphora . fromScope) b
hasNakedBlockAnaphora (CoreMeta _ m e) =
  hasNakedBlockAnaphora m || hasNakedBlockAnaphora e
hasNakedBlockAnaphora CoreBlock{} = False
hasNakedBlockAnaphora (CoreList _ exprs) = any hasNakedBlockAnaphora exprs
hasNakedBlockAnaphora (CoreArgTuple _ exprs) = any hasNakedBlockAnaphora exprs
hasNakedBlockAnaphora (CoreOperator _ _ _ expr) = hasNakedBlockAnaphora expr
hasNakedBlockAnaphora (CoreOpSoup _ exprs) = any hasNakedBlockAnaphora exprs
hasNakedBlockAnaphora (CoreApply _ f xs) = any hasNakedBlockAnaphora (f : xs)
hasNakedBlockAnaphora _ = False


-- | Transform any anaphoric blocks into Lambdas
transform :: (Anaphora SymbolicAnaphora a) => Bool -> CoreExp a -> CoreExp a
transform True expr = expr
transform False expr@(CoreLet smid bs b cl) =
  if any (hasNakedBlockAnaphora . fromScope) (b : map snd bs)
    then (bindAnaphora blockAnaphora . numberAnaphora blockAnaphora) expr
    else let b' = toScope $ transform False (fromScope b)
             bs' = map (second (toScope . transform False . fromScope)) bs
          in CoreLet smid bs' b' cl
transform False (CoreLambda smid inl ns b) =
  let b' = toScope (transform False (fromScope b))
   in CoreLambda smid inl ns b'
transform False expr@(CoreMeta smid m e) =
  let anaphoric = hasNakedBlockAnaphora expr
      m' = transform anaphoric m
      e' = transform anaphoric e
   in CoreMeta smid m' e'
transform False (CoreBlock smid e) =
  let anaphoric = hasNakedBlockAnaphora e
      e' = transform anaphoric e
   in CoreBlock smid e'
transform False (CoreList smid es) =
  let anaphoric = any hasNakedBlockAnaphora es
      es' = map (transform anaphoric) es
  in CoreList smid es'
transform False (CoreArgTuple smid es) =
  let anaphoric = any hasNakedBlockAnaphora es
      es' = map (transform anaphoric) es
  in CoreArgTuple smid es'
transform False (CoreOperator smid x p e) =
  let anaphoric = hasNakedBlockAnaphora e
      e' = transform anaphoric e
  in CoreOperator smid x p e'
transform False (CoreLookup smid o n d) =
  let anaphoric = hasNakedBlockAnaphora o
      o' = transform anaphoric o
      d' = transform anaphoric <$> d
  in CoreLookup smid o' n d'
transform False expr@(CoreApply smid f xs) =
  let anaphoric = hasNakedBlockAnaphora expr
      f' = transform anaphoric f
      xs' = map (transform anaphoric) xs
   in CoreApply smid f' xs'
transform False (CoreOpSoup smid xs) =
  let anaphoric = any hasNakedBlockAnaphora xs
      xs' = map (transform anaphoric) xs
   in CoreOpSoup smid xs'
transform False expr = expr
