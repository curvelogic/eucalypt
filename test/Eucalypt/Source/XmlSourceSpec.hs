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
  describe "Xml parser" $ do
    it "Parses simple xml" $
      parseXml "<xml>blah<a>foo</a><b c=\"d\">bar</b><e/></xml>" `shouldReturn`
      block
        [ element "_tag" $ str "xml"
        , element "_attrs" $ block []
        , element "_content" $
          corelist
            [ str "blah"
            , block
                [ element "_tag" $ str "a"
                , element "_attrs" $ block []
                , element "_content" $ corelist [str "foo"]
                ]
            , block
                [ element "_tag" $ str "b"
                , element "_attrs" $ block [element "c" $ str "d"]
                , element "_content" $ corelist [str "bar"]
                ]
            , block
                [ element "_tag" $ str "e"
                , element "_attrs" $ block []
                , element "_content" $ corelist []
                ]
            ]
        ]
