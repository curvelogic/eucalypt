{-|
Module      : Eucalypt.Stg.CallStack
Description : CallStack for diagnostics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}
module Eucalypt.Stg.CallStack where

import Data.Vector (Vector)

-- Structure to track (annotated) call stack
type CallStack = Vector String
