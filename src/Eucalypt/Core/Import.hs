{-# LANGUAGE RankNTypes #-}
{-|
Module      : Eucalypt.Core.Import
Description : Importing modules and data into core
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Import
    -- * Types and smart constructors
  ( ImportHandler(..)
  , nullImportHandler
  , ImportMap
    -- * Functions
  , applyAllImports
    -- * Exposed for tests
  , processImports
  ) where

import Bound
import Control.Monad.Trans
import Data.Bifunctor
import Data.Foldable (toList)
import qualified Data.Graph as G
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Eucalypt.Core.Syn
import Eucalypt.Core.Unit
import Eucalypt.Syntax.Input



-- | An import handler is something that can read 'Import's from
-- metadata in core. The driver will provide one.
data ImportHandler = ImportHandler {
    readImports :: forall v. CoreExp v -> [(Input, IO ())]
    -- ^ read an imports out of core syntax and convert to inputs
  , pruneImports :: forall v. CoreExp v -> CoreExp v
    -- ^ prune the import specifications from core syntax
}


-- | Map of input to their core expression, maintained during
-- processing of imports
type ImportMap = M.Map Input TranslationUnit



-- | A null import handler suitable for contexts (data) that cannot
-- contain imports.
nullImportHandler :: ImportHandler
nullImportHandler =
  ImportHandler {readImports = const [], pruneImports = id}



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
processImports ::
     (ToCoreBindingName a, Show a)
  => (Input -> CoreExp a) -- ^ load input from load cache
  -> ImportHandler        -- ^ for reading import metadata
  -> CoreExp a            -- ^ core for import processing
  -> CoreExp a            -- ^ core with imports inserted
processImports load handler (CoreMeta smid m body) =
  case readImports handler m of
    imports ->
      CoreMeta smid (pruneImports handler m) $
      foldr
        (rebody . load . fst)
        (processImports load handler body)
        imports
processImports load handler (CoreLet smid bs b _) = CoreLet smid bs' b' OtherLet
  where
    b' = f b
    bs' = map (second f) bs
    f = transScope (processImports (liftLoad load) handler)
processImports load handler (CoreLambda smid i ns b) = CoreLambda smid i ns b'
  where
    b' = f b
    f = transScope (processImports (liftLoad load) handler)
processImports load handler (CoreLookup smid obj n d) =
  CoreLookup
    smid
    (processImports load handler obj)
    n
    (processImports load handler <$> d)
processImports load handler (CoreList smid els) =
  CoreList smid $ map (processImports load handler) els
processImports load handler (CoreBlock smid l) =
  CoreBlock smid $ processImports load handler l
processImports load handler (CoreArgTuple smid as) =
  CoreArgTuple smid $ map (processImports load handler) as
processImports load handler (CoreApply smid f xs) =
  CoreApply
    smid
    (processImports load handler f)
    (map (processImports load handler) xs)
processImports load handler (CoreOpSoup smid els) =
  CoreOpSoup smid $ map (processImports load handler) els
processImports load handler (CoreOperator smid x p e) =
  CoreOperator smid x p $ processImports load handler e
processImports _ _ expr = expr



-- | Process the imports in a translation unit
processUnit ::
     (Input -> CoreExpr) -> ImportHandler -> TranslationUnit -> TranslationUnit
processUnit load handler u@TranslationUnit {truCore = body} =
  u {truCore = processImports load handler body}



-- | Process all imports in topologically sorted order unless there
-- are cycles, in which case return the inputs involved in the cycles
-- (in Left)
applyAllImports ::
     ImportHandler            -- ^ for reading imports
  -> ImportMap                -- ^ of inputs to core
  -> Either [Input] ImportMap -- ^ cyclic inputs or processed map
applyAllImports handler unitMap =
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
    processInput m input =
      M.update (return . processUnit (toLoadFn m) handler) input m
    cyclicInputs =
      (map toInput . mconcat . filter isCycle . map toList . G.scc) graph
    isCycle cc = length cc > 1 || (minimum cc, minimum cc) `elem` G.edges graph
