{-# LANGUAGE RankNTypes #-}
{-|
Module      : Eucalypt.Core.Verify
Description : Checks to run prior to evaluation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Verify where

import Bound
import Data.Foldable
import Eucalypt.Core.Error
import Eucalypt.Core.Recursion
import Eucalypt.Core.Syn


-- | A final pass to mark any unresolved variables and then clean out
-- any that appear in soft contexts where they do not indicate errors.
cleanEvaluand :: Show a => CoreExp a -> CoreExp a
cleanEvaluand = cleanDynamicFallbacks . markUnresolved



-- | Mark any unresolved variables so we can report over them
markUnresolved :: Show a => CoreExp a -> CoreExp a
markUnresolved expr = expr >>= CoreUnresolved 0 . show



-- | Eliminate any unresolved variables which are defaults in dynamic
-- lookups.
cleanDynamicFallbacks :: CoreExp a -> CoreExp a
cleanDynamicFallbacks (CoreLookup smid obj k (Just CoreUnresolved {})) =
  CoreLookup smid obj k Nothing
cleanDynamicFallbacks expr = walk cleanDynamicFallbacks expr



-- | Run all the check functions throughout the tree
runChecks :: Show b => CoreExp b -> [CoreError]
runChecks = verify cleanCore



-- | Apply a check function to every level in the syntax tree
verify ::
     Show b
  => (forall a. Show a =>
                  (CoreExp a -> [CoreError]))
  -> CoreExp b
  -> [CoreError]
verify f e@(CoreLet _ bs b _) =
  let shallow = f e
      deep = fold (verify f (unscope b) : map (verify f . unscope . snd) bs)
  in shallow ++ deep
verify f e@(CoreLambda _ _ _ b) =
  let shallow = f e
      deep = verify f (unscope b)
   in shallow ++ deep
verify f e@(CoreMeta _ m expr) =
  f e ++ verify f m ++ verify f expr
verify f e@(CoreBlock _ expr) =
  f e ++ verify f expr
verify f e@(CoreList _ exprs) =
  f e ++ concatMap (verify f) exprs
verify f e@(CoreArgTuple _ exprs) =
  f e ++ concatMap (verify f) exprs
verify f e@(CoreOperator _ _ _ expr) =
  f e ++ verify f expr
verify f e = f e


cleanCore :: Show a => CoreExp a -> [CoreError]
cleanCore e = noSoup e ++ noCoreName e ++ noEliminated e ++ notUnresolved e



noSoup :: Show a => CoreExp a -> [CoreError]
noSoup o@(CoreOpSoup _ _) = [(VerifyOperatorsFailed . CoreExpShow) o]
noSoup _ = []



noEliminated :: Show a => CoreExp a -> [CoreError]
noEliminated o@CoreEliminated = [(VerifyNoEliminated . CoreExpShow) o]
noEliminated _ = []



noCoreName :: Show a => CoreExp a -> [CoreError]
noCoreName o@CoreName{} = [(VerifyNamesFailed . CoreExpShow) o]
noCoreName _ = []


notUnresolved :: Show a => CoreExp a -> [CoreError]
notUnresolved o@CoreUnresolved{} = [(VerifyUnresolvedVar . CoreExpShow) o]
notUnresolved _ = []
