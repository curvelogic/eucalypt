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
runChecks :: Show b => CoreExp b -> [EvaluationError]
runChecks expr = verify noSoup expr ++ map (VerifyUnresolvedVar . show) (toList expr)

-- | Apply a check function to every level in the syntax tree
verify ::
     Show b
  => (forall a. Show a =>
                  (CoreExp a -> [EvaluationError]))
  -> CoreExp b
  -> [EvaluationError]
verify f e@(CoreLet bs b) =
  let shallow = f e
      deep = foldMap id (verify f (unscope b) : map (verify f . unscope . snd) bs)
  in shallow ++ deep
verify f e@(CoreLambda _ b) =
  let shallow = f e
      deep = verify f (unscope b)
   in shallow ++ deep
verify f e@(CoreMeta m expr) =
  f e ++ verify f m ++ verify f expr
verify f e@(CoreTraced expr) =
  f e ++ verify f expr
verify f e@(CoreChecked ck expr) =
  f e ++ verify f ck ++ verify f expr
verify f e@(CoreBlock expr) =
  f e ++ verify f expr
verify f e@(CoreList exprs) =
  f e ++ concatMap (verify f) exprs
verify f e@(CoreArgTuple exprs) =
  f e ++ concatMap (verify f) exprs
verify f e@(CoreOperator _ _ expr) =
  f e ++ verify f expr
verify f e = f e

noSoup :: Show a => CoreExp a -> [EvaluationError]
noSoup o@(CoreOpSoup _) = [(VerifyOperatorsFailed . CoreExpShow) o]
noSoup _ = []
