{-# LANGUAGE RankNTypes #-}
{-|
Module      : Eucalypt.Core.Recursion
Description : Combinators in the style of recursion schemes for @CoreExp@
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}


module Eucalypt.Core.Recursion where

import Bound
import Data.Bifunctor (second)
import Eucalypt.Core.Syn

transScope ::
     (Monad f1, Monad f2)
  => (f2 (Var b1 a1) -> f1 (Var b2 a2))
  -> Scope b1 f2 a1
  -> Scope b2 f1 a2
transScope f = toScope . f . fromScope


walk :: (forall b . (CoreExp b -> CoreExp b)) -> CoreExp a -> CoreExp a
walk _ (CoreVar smid a) = CoreVar smid a
walk f (CoreLet smid bs b cl) =
  let f' = transScope f
   in CoreLet smid (map (second f') bs) (f' b) cl
walk _ (CoreBuiltin smid n) = CoreBuiltin smid n
walk _ (CorePrim smid p) = CorePrim smid p
walk f (CoreLookup smid e n d) = CoreLookup smid (f e) n (f <$> d)
walk f (CoreList smid es) = CoreList smid (map f es)
walk f (CoreBlock smid e) = CoreBlock smid (f e)
walk f (CoreMeta smid m e) = CoreMeta smid (f m) (f e)
walk f (CoreOpSoup smid es) = CoreOpSoup smid (map f es)
walk f (CoreLambda smid i n e) = CoreLambda smid i n (transScope f e)
walk f (CoreOperator smid x p e) = CoreOperator smid x p (f e)
walk f (CoreArgTuple smid es) = CoreArgTuple smid (map f es)
walk f (CoreApply smid g es) = CoreApply smid (f g) (map f es)
walk _ expr = expr
