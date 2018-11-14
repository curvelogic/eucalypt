{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Eucalypt.Stg.CallStack
Description : CallStack for diagnostics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}
module Eucalypt.Stg.CallStack where

import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Eucalypt.Stg.Syn
import Eucalypt.Core.SourceMap
  ( HasSourceMapIds(..)
  , SMID
  , SourceMap
  , lookupSource
  )
import qualified Eucalypt.Reporting.Location as L
import qualified Text.PrettyPrint as P

-- Structure to track (annotated) call stack
newtype CallStack = CallStack { entries :: Vector (String, SMID)}
  deriving (Eq, Show, Semigroup, Monoid)

-- Add a new entry to a call stack
addEntry :: (String, SMID) -> CallStack -> CallStack
addEntry s (CallStack cs) = CallStack (cs `Vector.snoc` s)

instance StgPretty CallStack where
  prettify (CallStack cs) =
    if Vector.null cs
      then P.empty
      else P.brackets
             (P.hcat (P.punctuate (P.char '>') (map (P.text . fst) (toList cs))))

instance HasSourceMapIds CallStack where
  toSourceMapIds (CallStack v) = map snd . Vector.toList . Vector.reverse $ v


-- | Using a SourceMap, resolve SMIDs to allow the call stack to be
-- reportable in error messages.
resolveSMIDs :: CallStack -> SourceMap -> [(String, Maybe L.SourceSpan)]
resolveSMIDs CallStack {..} sm =
  toList $ Vector.reverse $ Vector.map (second (lookupSource sm)) entries
