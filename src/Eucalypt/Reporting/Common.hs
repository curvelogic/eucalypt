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


width :: Int
width = 80

-- | Format a title for the exception type
title :: String -> Doc
title s = text $ "- " ++ s ++ " " ++ replicate (width - 3 - length s) '-'


-- | Format title and exception message
standardReport :: String -> String -> Doc
standardReport t msg = title t $$ text "" $$ text msg
