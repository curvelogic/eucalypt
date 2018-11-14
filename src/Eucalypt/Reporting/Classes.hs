{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Eucalypt.Reporting.Classes
Description : Type classes to implement for error reporting
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Reporting.Classes where

import Eucalypt.Reporting.Location as L
import Text.PrettyPrint as P


class Reportable a where
  -- | Location in SourceCode
  code :: a -> Maybe L.SourceSpan
  code _ = Nothing

  -- | Formatted error report
  report :: a -> P.Doc

  -- | Call trace
  callTrace :: a -> Maybe [(String, Maybe L.SourceSpan)]
  callTrace _ = Nothing

  -- | Suggestions
  suggestions :: a -> [String]
  suggestions _ = []
