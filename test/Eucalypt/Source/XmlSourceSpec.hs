{-# LANGUAGE OverloadedStrings #-}

module Eucalypt.Source.XmlSourceSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.AnonSyn
import Eucalypt.Source.XmlSource
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Xml parser" $
  it "Parses simple xml" $
  parseXml "<xml>blah<a>foo</a><b c=\"d\">bar</b><e/></xml>" `shouldReturn`
  corelist
    [ sym "xml"
    , block []
    , str "blah"
    , corelist [sym "a", block [], str "foo"]
    , corelist [sym "b", block [element "c" $ str "d"], str "bar"]
    , corelist [sym "e", block []]
    ]
