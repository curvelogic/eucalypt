{-|
Module      : Eucalypt.Core.Import
Description : Importing modules and data into core
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Import where

import Bound
import Control.Monad.Trans
import Data.Foldable (toList)
import qualified Data.Graph as G
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Eucalypt.Core.Metadata
import Eucalypt.Core.Syn
import Eucalypt.Core.Unit
import Eucalypt.Syntax.Input
import Data.Bifunctor

-- | Convert a function on expressions to a function on scopes
transScope ::
     (CoreExp (Var b a) -> CoreExp (Var b a))
  -> Scope b CoreExp a
  -> Scope b CoreExp a
transScope f = toScope . f . fromScope

-- | Lift a function that loads core from inputs to a function that
-- loads a version nested one level deeper in binding.
liftLoad :: (Input -> CoreExp a) -> (Input -> CoreExp (Var b a))
liftLoad load = fromScope . lift . load

-- | Pure import-splicing function, assumes that we have preloaded
-- core expression for each input.
--
--
processImports ::
     (ToCoreBindingName a, Show a)
  => (Input -> CoreExp a)
  -> CoreExp a
  -> CoreExp a
processImports load expr@(CoreMeta smid m body) =
  case importsFromMetadata m of
    Just imports ->
      CoreMeta smid (pruneImports m) $
      foldr (\i e -> rebody (load i) e) (processImports load body) imports
    Nothing -> expr
processImports load (CoreLet smid bs b _) = CoreLet smid bs' b' OtherLet
  where
    b' = f b
    bs' = map (second f) bs
    f = transScope (processImports (liftLoad load))
processImports load (CoreLambda smid i ns b) = CoreLambda smid i ns b'
  where
    b' = f b
    f = transScope (processImports (liftLoad load))
processImports load (CoreLookup smid obj n d) =
  CoreLookup smid (processImports load obj) n (processImports load <$> d)
processImports load (CoreList smid els) =
  CoreList smid $ map (processImports load) els
processImports load (CoreBlock smid l) =
  CoreBlock smid $ processImports load l
processImports load (CoreArgTuple smid as) =
  CoreArgTuple smid $ map (processImports load) as
processImports load (CoreApply smid f xs) =
  CoreApply smid (processImports load f) (map (processImports load) xs)
processImports load (CoreOpSoup smid els) =
  CoreOpSoup smid $ map (processImports load) els
processImports  load (CoreOperator smid x p e) =
  CoreOperator smid x p $ processImports load e
processImports _ expr = expr



-- | Process the imports in a translation unit
processUnit :: (Input -> CoreExpr) -> TranslationUnit -> TranslationUnit
processUnit load u@TranslationUnit {truCore = body} =
  u {truCore = processImports load body}



-- | Process all imports in topologically sorted order unless there
-- are cycles, in which case return the inputs involved in the cycles
-- (in Left)
applyAllImports ::
     M.Map Input TranslationUnit -> Either [Input] (M.Map Input TranslationUnit)
applyAllImports unitMap =
  if (not . null) cyclicInputs
    then Left cyclicInputs
    else Right $ foldl processInput unitMap sortedInputs
  where
    (graph, getVertex) = G.graphFromEdges' $ map edgeSpec $ M.assocs unitMap
    edgeSpec (k, v) = (k, k, toList $ truImports v)
    sortedInputs = map toInput $ (reverse . G.topSort) graph
    toInput v =
      case getVertex v of
        (i, _, _) -> i
    toLoadFn m k =
      truCore $ fromMaybe (error $ "no such key: " ++ show k) (M.lookup k m)
    processInput m input = M.update (return . processUnit (toLoadFn m)) input m
    cyclicInputs =
      (map toInput . mconcat . filter isCycle . map toList . G.scc) graph
    isCycle cc = length cc > 1 || (minimum cc, minimum cc) `elem` G.edges graph
