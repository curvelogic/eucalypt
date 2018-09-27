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
import Data.Maybe (fromJust)
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
processImports load expr@(CoreMeta m body) =
  case importsFromMetadata m of
    Just imports ->
      CoreMeta (pruneImports m) $
      foldr (\i e -> rebody (load i) e) (processImports load body) imports
    Nothing -> expr
processImports load (CoreLet bs b) = CoreLet bs' b'
  where
    b' = f b
    bs' = map (second f) bs
    f = transScope (processImports (liftLoad load))
processImports load (CoreLambda ns b) = CoreLambda ns b'
  where
    b' = f b
    f = transScope (processImports (liftLoad load))
processImports load (CoreLookup obj n) =
  CoreLookup (processImports load obj) n
processImports load (CoreList els) =
  CoreList $ map (processImports load) els
processImports load (CoreBlock l) =
  CoreBlock $ processImports load l
processImports load (CoreArgTuple as) =
  CoreArgTuple $ map (processImports load) as
processImports load (CoreApply f xs) =
  CoreApply (processImports load f) (map (processImports load) xs)
processImports load (CoreOpSoup els) =
  CoreOpSoup $ map (processImports load) els
processImports load (CoreOperator x p e) =
  CoreOperator x p $ processImports load e
processImports _ expr = expr



-- | Process the imports in a translation unit
processUnit :: (Input -> CoreExpr) -> TranslationUnit -> TranslationUnit
processUnit load u@TranslationUnit {truCore = body} =
  u {truCore = processImports load body}



-- | Process all imports in topologically sorted order
applyAllImports :: M.Map Input TranslationUnit -> M.Map Input TranslationUnit
applyAllImports unitMap = foldl processInput unitMap sortedInputs
  where
    (graph, getVertex) = G.graphFromEdges' $ map edgeSpec $ M.assocs unitMap
    edgeSpec (k, v) = (k, k, toList $ truImports v)
    sortedInputs = map (toInput . getVertex) $ (reverse . G.topSort) graph
    toInput (i, _, _) = i
    toLoadFn m k = truCore $ fromJust (M.lookup k m)
    processInput ::
         M.Map Input TranslationUnit -> Input -> M.Map Input TranslationUnit
    processInput m input = M.update (return . processUnit (toLoadFn m)) input m
