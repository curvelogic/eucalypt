{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Eucalypt.Stg.Tags
Description : Predefined constructor tags for STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Tags where

import Eucalypt.Stg.Syn (LambdaForm, Tag, standardConstructor)

stgNil :: Tag
stgNil = 0

pattern TagNil :: Tag
pattern TagNil = 0

stgCons :: Tag
stgCons = 1

pattern TagCons :: Tag
pattern TagCons = 1

stgBlock :: Tag
stgBlock = 2

pattern TagBlock :: Tag
pattern TagBlock = 2

stgUnit :: Tag
stgUnit = 3

pattern TagUnit :: Tag
pattern TagUnit = 3

stgTrue :: Tag
stgTrue = 4

pattern TagTrue :: Tag
pattern TagTrue = 4

stgFalse :: Tag
stgFalse = 5

pattern TagFalse :: Tag
pattern TagFalse = 5

boolTag :: Bool -> Tag
boolTag True = stgTrue
boolTag False = stgFalse

nilConstructor :: LambdaForm
nilConstructor = standardConstructor 0 stgNil

consConstructor :: LambdaForm
consConstructor = standardConstructor 2 stgCons

blockConstructor :: LambdaForm
blockConstructor = standardConstructor 1 stgBlock

unitConstructor :: LambdaForm
unitConstructor = standardConstructor 0 stgUnit

trueConstructor :: LambdaForm
trueConstructor = standardConstructor 0 stgTrue

falseConstructor :: LambdaForm
falseConstructor = standardConstructor 0 stgFalse
