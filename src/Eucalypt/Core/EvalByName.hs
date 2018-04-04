{-|
Module      : Eucalypt.Core.EvalByName
Description : A simple call-by-name evaluator to get going with...
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.EvalByName
  where

import Bound
import Eucalypt.Core.Syn

-- | Evaluate to WHNF
whnf :: CoreExp a -> CoreExp a
whnf e@CoreVar{} = e
whnf e@CoreLam{} = e
whnf e@CoreBlock{} = e
whnf e@CoreList{} = e
whnf e@CorePrim{} = e
whnf (CoreApp f x) = case whnf f of
  CoreLam e -> whnf (instantiate1 x e)
  f' -> CoreApp f' x
whnf (CoreLet bs b) = whnf (inst b)
  where es = map inst bs
        inst = instantiate (es !!)
-- TODO: lookup
