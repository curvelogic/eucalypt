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
import qualified Text.Megaparsec.Pos as M

-- | Wrapper for parsec SourcePos so we can cleanly JSONify
newtype SourcePosition =
  SourcePosition SourcePos
  deriving (Eq, Show, Generic, Ord)

instance ToJSON SourcePosition where
  toJSON (SourcePosition p) =
    String $
    pack
      (sourceName p ++
       ":" ++ show (sourceLine p) ++ show (sourceColumn p))

-- | Text span from start to end
type SourceSpan = (SourcePosition, SourcePosition)

-- | A parsed expression supplemented with location information
data Located a = Located
  { location :: SourceSpan
  , locatee :: a
  } deriving (Eq, Show, Generic, ToJSON)

-- | Simple typeclass for expression that we can strip location
-- information from (mainly for testing).
class HasLocation a where
  stripLocation :: a -> a

-- | Anything we can strip location from, we can also strip location
-- from a located variation of it...
instance HasLocation a => HasLocation (Located a) where
  stripLocation Located{locatee=x} = Located{location=nowhere, locatee=stripLocation x}

-- | Construct a new position
pos :: SourceName -> Line -> Column -> SourcePosition
pos n l c = spos $ newPos n l c

-- | Convert from parsec position
spos :: SourcePos -> SourcePosition
spos = SourcePosition

-- | Convert from megaparsec position
mpos :: M.SourcePos -> SourcePosition
mpos m
  = SourcePosition (newPos
                    (M.sourceName m)
                    ((M.unPos . M.sourceLine) m)
                    ((M.unPos . M.sourceColumn) m))

-- | Add location data to primary expression
at :: SourceSpan -> a -> Located a
at sp expr = Located {location = sp, locatee = expr}

-- | Update the location information for an expression
move :: SourceSpan -> Located a -> Located a
move sp expr = expr {location = sp}

-- | Merge to spans into a larger one (assumes source file is the
-- same)
merge :: SourceSpan -> SourceSpan -> SourceSpan
merge a b = (min (fst a) (fst b), max (snd a) (snd b))

-- | When expressions don't come from source
nowherePos :: SourcePosition
nowherePos = pos "<<nowhere>>" 0 0

-- | Fake span
nowhere :: SourceSpan
nowhere = (nowherePos, nowherePos)
