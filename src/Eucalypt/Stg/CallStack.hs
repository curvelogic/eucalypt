{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module      : Eucalypt.Stg.CallStack
Description : CallStack for diagnostics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}
module Eucalypt.Stg.CallStack where

import Data.Foldable (toList)
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Eucalypt.Stg.Syn
import qualified Text.PrettyPrint as P

-- Structure to track (annotated) call stack
newtype CallStack = CallStack { entries :: Vector String}
  deriving (Eq, Show, Semigroup, Monoid)

-- Add a new entry to a call stack
addEntry :: String -> CallStack -> CallStack
addEntry s (CallStack cs) = CallStack (cs `Vector.snoc` s)

instance StgPretty CallStack where
  prettify (CallStack cs) =
    if Vector.null cs
      then P.empty
      else P.brackets
             (P.hcat (P.punctuate (P.char '>') (map P.text (toList cs))))
