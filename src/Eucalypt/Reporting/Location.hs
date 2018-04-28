{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-|
Module      : Eucalypt.Reporting.Location
Description : Sorce code location references
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Reporting.Location where

import Data.Aeson
import Data.Text (pack)
import GHC.Generics
import Text.Parsec.Pos
  ( Column
  , Line
  , SourceName
  , SourcePos
  , newPos
  , sourceColumn
  , sourceLine
  , sourceName
  )

-- | Wrapper for parsec SourcePos so we can cleanly JSONify
newtype SourcePosition =
  SourcePosition SourcePos
  deriving (Eq, Show, Generic)

instance ToJSON SourcePosition where
  toJSON (SourcePosition p) =
    String $
    pack
      (sourceName p ++
       ":" ++ (show $ sourceLine p) ++ (show $ sourceColumn p))

-- | Text span from start to end
type SourceSpan = (SourcePosition, SourcePosition)

-- | A parsed expression supplemented with location information
data Located a = Located
  { location :: SourceSpan
  , locatee :: a
  } deriving (Eq, Show, Generic, ToJSON)

-- | Construct a new position
pos :: SourceName -> Line -> Column -> SourcePos
pos n l c = newPos n l c

-- | Add location data to primary expression
at :: SourceSpan -> a -> Located a
at sp expr = Located {location = sp, locatee = expr}

-- | Update the location information for an expression
move :: SourceSpan -> Located a -> Located a
move sp expr = expr {location = sp}
