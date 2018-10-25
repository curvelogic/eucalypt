{-# LANGUAGE OverloadedStrings #-}

module Eucalypt.Source.TextSourceSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.AnonSyn
import Eucalypt.Source.TextSource
import Data.Text
import Data.Text.Encoding
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Text source" $
  it "reads as list" $
  parseTextLines (encodeUtf8 $ pack "one\ntwo\nthree\nξ") `shouldReturn`
  corelist [str "one", str "two", str "three", str "ξ"]
