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
