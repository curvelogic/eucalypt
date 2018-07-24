{-|
Module      : Eucalypt.Core.Syn
Description : Facilities for handling anaphoric params in Eucalypt syntax
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.Anaphora where

-- | Various types of anaphoric references appear in the Eucalypt
-- syntax, notably expression anaphora ('_', '_0', '_1'), and string
-- anaphora ({}, {0}, {1}).
--
-- This typeclass allows us to treat them similarly
class (Eq a, Show a) => Anaphora a where

  -- | The unnumberedAnaphor for this type (e.g. '_') where the index
  -- is implicitly inferred from sequenc
  unnumberedAnaphor :: a

  -- | Is a an anaphor (numbered or not)
  isAnaphor :: a -> Bool

  -- | Read the index from an anaphor if it is numbered
  toNumber :: a -> Maybe Int

  -- | Create an anaphor for the specified number
  fromNumber :: Int -> a

  -- | A name for rendering
  toName :: a -> String
