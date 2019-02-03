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
import Eucalypt.Stg.Pretty
import Eucalypt.Core.SourceMap
  ( HasSourceMapIds(..)
  , SMID
  , SourceMap
  , lookupSource
  )
import qualified Eucalypt.Reporting.Location as L
import qualified Text.PrettyPrint as P

-- | Structure to track (annotated) call stack
newtype CallStack = CallStack { entries :: [(String, SMID)]}
  deriving (Eq, Show, Semigroup, Monoid)

-- | Add a new entry to a call stack
addEntry :: (String, SMID) -> CallStack -> CallStack
addEntry s (CallStack cs) = CallStack (s : cs)

instance StgPretty CallStack where
  prettify (CallStack cs) =
    if null cs
      then P.empty
      else P.brackets
             (P.hcat (P.punctuate (P.char '>') (map (P.text . fst) (reverse cs))))


instance HasSourceMapIds CallStack where
  toSourceMapIds (CallStack v) = map snd v


-- | Using a SourceMap, resolve SMIDs to allow the call stack to be
-- reportable in error messages.
resolveSMIDs :: CallStack -> SourceMap -> [(String, Maybe L.SourceSpan)]
resolveSMIDs CallStack {..} sm = map (second (lookupSource sm)) entries
