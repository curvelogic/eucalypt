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
import Eucalypt.Core.Syn

-- | Run all the check functions throughout the tree
runChecks :: Show b => CoreExp b -> [CoreError]
runChecks expr =
  verify cleanCore expr ++ map (VerifyUnresolvedVar . show) (toList expr)



-- | Apply a check function to every level in the syntax tree
verify ::
     Show b
  => (forall a. Show a =>
                  (CoreExp a -> [CoreError]))
  -> CoreExp b
  -> [CoreError]
verify f e@(CoreLet _ bs b) =
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
cleanCore e = noSoup e ++ noCoreName e ++ noEliminated e



noSoup :: Show a => CoreExp a -> [CoreError]
noSoup o@(CoreOpSoup _ _) = [(VerifyOperatorsFailed . CoreExpShow) o]
noSoup _ = []



noEliminated :: Show a => CoreExp a -> [CoreError]
noEliminated o@CoreEliminated = [(VerifyNoEliminated . CoreExpShow) o]
noEliminated _ = []



noCoreName :: Show a => CoreExp a -> [CoreError]
noCoreName o@CoreName{} = [(VerifyNamesFailed . CoreExpShow) o]
noCoreName _ = []
