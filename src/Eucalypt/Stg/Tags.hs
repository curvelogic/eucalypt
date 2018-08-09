{-|
Module      : Eucalypt.Stg.Tags
Description : Predefined constructor tags for STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Tags where

import Eucalypt.Stg.Syn (Tag)

stgNil :: Tag
stgNil = 0

stgCons :: Tag
stgCons = 1

stgBlock :: Tag
stgBlock = 2

stgUnit :: Tag
stgUnit = 3
