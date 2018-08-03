{-|
Module      : Eucalypt.Stg.Syn
Description : Tests for STG syntax helpers
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.SynSpec (main, spec)
where

import Eucalypt.Stg.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "isContextualRef" $
  it "classifies" $
  isContextualRef (Global "STR") `shouldBe` False
