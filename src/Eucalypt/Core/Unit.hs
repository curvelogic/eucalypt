{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Core.Unit
Description : Core and metadata for a translation unit
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Unit
  -- * Types and constructors
  ( TranslationUnit_(..)
  , TranslationUnit
  , dataUnit
  , specialUnit
  -- * Functions
  , applyName
  , resetActions
  , mergeTranslationUnits
  ) where

import qualified Data.Set as S
import Eucalypt.Syntax.Input
import Eucalypt.Core.SourceMap
import Eucalypt.Core.Syn 
import Eucalypt.Core.Target (TargetSpec, prefixPath)

-- | The results of translation
data TranslationUnit_ a = TranslationUnit
  { truCore :: a
    -- ^ the core expression for evaluation
  , truInput :: Maybe Input
    -- ^ the input representing this (if we have one)
  , truTargets :: [TargetSpec]
    -- ^ targets discovered in the source
  , truImports :: S.Set Input
    -- ^ imports yet to be resolved and processed
  , truSourceMap :: SourceMap
    -- ^ source map of ids to locations
  , truPendingActions :: [IO ()]
    -- ^ IO actions required for import processing (i.e. cache repos)
  } deriving (Functor)

type TranslationUnit = TranslationUnit_ CoreExpr



-- | Create a data unit (as a unit from an input with no targets or imports)
dataUnit :: Input -> CoreExpr -> TranslationUnit
dataUnit input expr = TranslationUnit expr (Just input) [] mempty mempty mempty



-- | Create a special unit for programatically constructed core, like
-- the IO unit, (not from real input, no targets / imports)
specialUnit :: CoreExpr -> TranslationUnit
specialUnit expr = TranslationUnit expr Nothing [] mempty mempty mempty



-- | Apply a name to the translation unit. This alters the core
-- expression and adjusts target paths appropriately
applyName :: CoreBindingName -> TranslationUnit -> TranslationUnit
applyName n TranslationUnit {..} =
  TranslationUnit
    { truCore = newCore
    , truInput = truInput
    , truTargets = newTargets
    , truImports = truImports
    , truSourceMap = truSourceMap
    , truPendingActions = truPendingActions
    }
  where
    newCore =
      anon letexp [(n, truCore)] (anon block [anon element n (anon var n)])
    newTargets = map (prefixPath n) truTargets



-- | Take the actions out of the unit for processing
resetActions :: TranslationUnit -> TranslationUnit
resetActions unit = unit {truPendingActions = mempty}



-- | Merge translation units into one such that earlier units can bind
-- unbound names in the latter. The body of the resulting let comes
-- from the last unit.
--
-- This is used to merge the various inputs specified on the command
-- line.
mergeTranslationUnits :: [TranslationUnit] -> TranslationUnit
mergeTranslationUnits ts =
  TranslationUnit
    { truCore = mergeUnits $ map truCore ts
    , truInput = last $ map truInput ts
    , truTargets = concatMap truTargets ts
    , truImports = foldMap truImports ts
    , truSourceMap = mconcat $ map truSourceMap ts
    , truPendingActions = mconcat $ map truPendingActions ts
    }
