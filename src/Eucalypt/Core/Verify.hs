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
runChecks = foldCoreExpr cleanCore



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
