{-# LANGUAGE OverloadedStrings #-}
module Eucalypt.Reporting.CodeSpec (main, spec)
where

import qualified Data.ByteString as B
import Data.List (intercalate)
import Eucalypt.Reporting.Code
import Test.Hspec
import Text.PrettyPrint as P

main :: IO ()
main = hspec spec

sample1 :: B.ByteString
sample1 = "foo\nbar\nbaz\nquux\n"

result1 :: String
result1 =
  intercalate "\n" ["   |", "   |", " 3 | quux", "   |   ^", "   | --/"]

result2 :: String
result2 =
  intercalate "\n" ["   |", "   |", " 2 | baz", "   | ^^", "   |"]

result3 :: String
result3 =
  intercalate "\n" ["   |", " 1 | bar", "   | ...", "   | ...", " 3 | quux", "   |"]

spec :: Spec
spec =
  describe "Code formatting" $ do
  it "formats points" $
    P.render (formatPoint sample1 3 3) `shouldBe` result1
  it "formats single line spans" $
    P.render (formatSingleLine sample1 2 0 2) `shouldBe` result2
  it "formats multiline regions" $
    P.render (formatRegion sample1 1 0 3 0) `shouldBe` result3
