{-|
Module      : Eucalypt.Reporting.Common
Description : Common formatting tools for reporting errors
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Reporting.Common where

import Text.PrettyPrint


-- | Format a title for the exception type
title :: String -> Doc
title = text


-- | Format title and exception message
standardReport :: String -> String -> Doc
standardReport t msg = title t $$ text msg
