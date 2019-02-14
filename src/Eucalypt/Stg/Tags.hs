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

nilConstructor :: LambdaForm
nilConstructor = standardConstructor 0 stgNil

stgCons :: Tag
stgCons = 1

pattern TagCons :: Tag
pattern TagCons = 1

consConstructor :: LambdaForm
consConstructor = standardConstructor 2 stgCons

stgBlock :: Tag
stgBlock = 2

pattern TagBlock :: Tag
pattern TagBlock = 2

blockConstructor :: LambdaForm
blockConstructor = standardConstructor 1 stgBlock

stgUnit :: Tag
stgUnit = 3

pattern TagUnit :: Tag
pattern TagUnit = 3

unitConstructor :: LambdaForm
unitConstructor = standardConstructor 0 stgUnit

stgTrue :: Tag
stgTrue = 4

pattern TagTrue :: Tag
pattern TagTrue = 4

trueConstructor :: LambdaForm
trueConstructor = standardConstructor 0 stgTrue

stgFalse :: Tag
stgFalse = 5

pattern TagFalse :: Tag
pattern TagFalse = 5

falseConstructor :: LambdaForm
falseConstructor = standardConstructor 0 stgFalse

boolTag :: Bool -> Tag
boolTag True = stgTrue
boolTag False = stgFalse

stgZDT :: Tag
stgZDT = 6

pattern TagZDT :: Tag
pattern TagZDT = 6

zdtConstructor :: LambdaForm
zdtConstructor = standardConstructor 0 stgZDT

stgIOSMBlock :: Tag
stgIOSMBlock = 7

pattern TagIOSMBlock :: Tag
pattern TagIOSMBlock = 7

iosmBlockConstructor :: LambdaForm
iosmBlockConstructor = standardConstructor 0 stgIOSMBlock
