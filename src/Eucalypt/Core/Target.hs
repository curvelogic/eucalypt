{-|
Module      : Eucalypt.Core.Target
Description : TargetSpec for identifying and managing evaluation targets
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Target where

import Eucalypt.Core.Syn

-- | Representation of an evaluation target, containing name,
-- documentation and path
data TargetSpec = TargetSpec
  { tgtName :: String
  , tgtDoc :: String
  , tgtPath :: [CoreBindingName]
  }
  deriving (Show, Eq)
