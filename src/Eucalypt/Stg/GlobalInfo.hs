{-|
Module      : Eucalypt.Stg.GlobalInfo
Description : GlobalInfo type for describing globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.GlobalInfo where

import Eucalypt.Stg.Syn

data Strictness = Strict | NonStrict
  deriving (Show, Eq)

data GlobalInfo = GlobalInfo
  { globalName :: String
  , globalCode :: LambdaForm
  , globalStrictness :: [Strictness]
  }
