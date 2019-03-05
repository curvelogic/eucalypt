module Eucalypt.Core.MetadataSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Metadata
import Eucalypt.Core.AnonSyn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Metadata normalisation" $ do
  it "Normalises strings to docs" $
    normaliseMetadata (str "x") `shouldBe` block [element "doc" $ str "x"]
  it "Normalises :main to a target" $
    normaliseMetadata (sym "main") `shouldBe` block [element "target" $ sym "main"]
  it "Normalises :suppress to export meta" $
    normaliseMetadata (sym "suppress") `shouldBe` block [element "export" $ sym "suppress"]
