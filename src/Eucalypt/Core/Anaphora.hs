{-# LANGUAGE MultiParamTypeClasses #-}
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
-- This typeclass allows us to treat them similarly.
--
-- 't' is the anaphor type (block, expression, string...)
-- 'a' is the binding name type that expresses the anaphor (e.g.
-- 'CoreBindingName' or 'Var')
class (Eq a, Show a) => Anaphora t a where

  -- | The unnumberedAnaphor for this type (e.g. '_') where the index
  -- is implicitly inferred from sequenc
  unnumberedAnaphor :: t -> a

  -- | Is a an anaphor (numbered or not)
  isAnaphor :: t -> a -> Bool

  -- | Read the index from an anaphor if it is numbered
  toNumber :: t -> a -> Maybe Int

  -- | Create an anaphor for the specified number
  fromNumber :: t -> Int -> a

  -- | A name for rendering
  toName :: t -> a -> String
