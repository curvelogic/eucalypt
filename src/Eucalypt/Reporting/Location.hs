{-# LANGUAGE DeriveGeneric, OverloadedLists, OverloadedStrings #-}
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
import qualified Text.Megaparsec.Pos as M

-- | Wrapper for parsec SourcePos so we can cleanly JSONify
newtype SourcePosition =
  SourcePosition M.SourcePos
  deriving (Eq, Show, Generic, Ord)

instance ToJSON SourcePosition where
  toJSON (SourcePosition p) =
    String $
    pack
      (M.sourceName p ++ ":" ++ show (M.sourceLine p) ++ "," ++ show (M.sourceColumn p))

-- | Text span from start to end
type SourceSpan = (SourcePosition, SourcePosition)

-- | A parsed expression supplemented with location information
data Located a = Located
  { location :: SourceSpan
  , locatee :: a
  } deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Located a) where
  toJSON Located {location = loc, locatee = a} =
    case toJSON a of
      Object o -> Object $ ["_at" .= str loc] <> o
      v -> object ["_at" .= str loc, "_value" .= v]
    where
      strPos p = show $ M.unPos p
      str (SourcePosition M.SourcePos { M.sourceName = n
                                      , M.sourceLine = lb
                                      , M.sourceColumn = cb
                                      }, SourcePosition M.SourcePos { M.sourceLine = le
                                                                    , M.sourceColumn = ce
                                                                    }) =
        pack
          (n ++
           ":" ++
           strPos lb ++ "," ++ strPos cb ++ "-" ++ strPos le ++ "," ++ strPos ce)

instance Functor Located where
  fmap f l@Located {locatee = x} = l {locatee = f x}

-- | Simple typeclass for expression that we can strip location
-- information from (mainly for testing).
class HasLocation a where
  stripLocation :: a -> a

-- | Anything we can strip location from, we can also strip location
-- from a located variation of it...
instance HasLocation a => HasLocation (Located a) where
  stripLocation Located{locatee=x} = Located{location=nowhere, locatee=stripLocation x}

-- | Construct a new position
pos :: FilePath -> Int -> Int -> SourcePosition
pos n l c = SourcePosition $ M.SourcePos n (M.mkPos l) (M.mkPos c)

-- | Convert from megaparsec position
mpos :: M.SourcePos -> SourcePosition
mpos = SourcePosition

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
nowherePos = SourcePosition $ M.SourcePos "<<nowhere>>" M.pos1 M.pos1

-- | Fake span
nowhere :: SourceSpan
nowhere = (nowherePos, nowherePos)
