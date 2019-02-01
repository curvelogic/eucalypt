{-|
Module      : Eucalypt.Stg.Pretty
Description : StgPretty typeclass for STG debug dumps
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Pretty where

import qualified Text.PrettyPrint as P

-- | Pretty printable syntax element
class StgPretty a where
  -- | Construct pretty print 'Doc' for a syntax element
  prettify :: a -> P.Doc
