{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Core.Unit
Description : Core and metadata for a translation unit
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Unit where

import Eucalypt.Core.Syn 
import Eucalypt.Core.Target (TargetSpec, prefixPath)

-- | The results of translation
data TranslationUnit = TranslationUnit
  { truCore :: CoreExpr
  , truTargets :: [TargetSpec]
  }


-- | Create a data unit (as a unit with no targets)
dataUnit :: CoreExpr -> TranslationUnit
dataUnit expr = TranslationUnit expr []

-- | Apply a name to the translation unit. This alters the core
-- expression and adjusts target paths appropriately
applyName :: CoreBindingName -> TranslationUnit -> TranslationUnit
applyName n TranslationUnit {..} =
  TranslationUnit {truCore = newCore, truTargets = newTargets}
  where
    newCore = letexp [(n, truCore)] (block [element n (var n)])
    newTargets = map (prefixPath n) truTargets

mergeTranslationUnits :: [TranslationUnit] -> TranslationUnit
mergeTranslationUnits ts =
  TranslationUnit
    { truCore = mergeUnits $ map truCore ts
    , truTargets = concatMap truTargets ts
    }
