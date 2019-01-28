{-|
Module      : Eucalypt.Core.Inliner
Description : Pass for inlining definitions
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Inliner where

import Data.Bifunctor
import Bound
import Eucalypt.Core.Recursion
import Eucalypt.Core.Syn


inline :: CoreExp a -> CoreExp a
inline = betaReduce . distribute . tagInlinables


-- | Distribute the definition down the tree to all call sites.
--
-- Dead definitions will remain at this stage.
distribute :: CoreExp a -> CoreExp a
distribute (CoreLet smid bs b cl) = CoreLet smid bindings body cl
  where
    bindingsToInline = map (selectScopeForInline . snd) bs
    body = instantiateSome bindingsToInline b
    bindings = map (second $ instantiateSome bindingsToInline) bs
distribute (CoreOperator _ _ _ e) = e
distribute e = walk distribute e



instantiateSome ::
     [Maybe (Scope Int CoreExp a)]
  -> Scope Int CoreExp a
  -> Scope Int CoreExp a
instantiateSome bs e = Scope $ unscope e >>= k
  where
    k v =
      case v of
        B b -> case bs !! b of
                 Nothing -> CoreVar 0 $ B b
                 Just sub -> distribute $ unscope sub
        F a -> CoreVar 0 $ F a



selectScopeForInline :: Scope Int CoreExp a -> Maybe (Scope Int CoreExp a)
selectScopeForInline (Scope e) = Scope <$> selectForInline e



selectForInline :: CoreExp a -> Maybe (CoreExp a)
selectForInline e =
  if isInlinable e
    then Just e
    else Nothing



isInlinable :: CoreExp a -> Bool
isInlinable (CoreLambda _ True _ _ ) = True
isInlinable CoreBuiltin{} = True
isInlinable CoreVar{} = True
isInlinable _ = False



isTransposition :: Scope Int CoreExp a -> Bool
isTransposition s = case unscope s of
  (CoreApply _ f xs) -> all isVar xs && isSimple f
  (CoreVar _ _) -> True
  _ -> False
  where
    isVar CoreVar{} = True
    isVar _ = False
    isSimple CoreBuiltin{} = True
    isSimple CoreVar{} = True
    isSimple _ = False



-- | A pass to tag all lambdas that are simple enough to be
-- distributed out to call sites.
tagInlinables :: CoreExp a -> CoreExp a
tagInlinables e@(CoreLambda smid False ns body) =
  if isTransposition body
    then CoreLambda smid True ns body
    else e
tagInlinables e = walk tagInlinables e



-- | Distribute lamba bodies out to call sites
betaReduce :: CoreExp a -> CoreExp a
betaReduce (CoreApply smid l@(CoreLambda _ inlineFlag ns body) xs) =
  if inlineFlag && length xs == length ns
    then betaReduce $ instantiate (map betaReduce xs !!) body
  else CoreApply smid (betaReduce l) (map betaReduce xs)
betaReduce e = walk betaReduce e
