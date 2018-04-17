{-# LANGUAGE OverloadedStrings #-}
module Eucalypt.Source.YamlSourceSpec
  ( main
  , spec
  ) where

import Eucalypt.Source.YamlSource
import Eucalypt.Core.Syn as S
import Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec =
  describe "Parses YAML" $ do
    it "Parses key value" $
      parseYamlData "a: !!int 1234" `shouldReturn`
      CoreBlock
        (CoreList [CoreList [CorePrim $ S.Symbol "a", CorePrim $ S.Int 1234]])
    it "Parses JSON data" $
      parseYamlData " { a: [1, 2, 3], b: {x: \"y\"} } " `shouldReturn`
      CoreBlock
        (CoreList
           [ CoreList
               [ CorePrim $ S.Symbol "a"
               , CoreList
                   [ CorePrim $ S.String "1"
                   , CorePrim $ S.String "2"
                   , CorePrim $ S.String "3"
                   ]
               ]
           , CoreList
               [ CorePrim $ S.Symbol "b"
               , CoreBlock
                   (CoreList
                      [ CoreList
                           [ CorePrim $ S.Symbol "x"
                           , CorePrim $ S.String "y"
                           ]
                      ])
               ]
           ])
