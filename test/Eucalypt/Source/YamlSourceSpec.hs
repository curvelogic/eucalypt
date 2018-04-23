{-# LANGUAGE OverloadedStrings #-}
module Eucalypt.Source.YamlSourceSpec
  ( main
  , spec
  ) where

import Eucalypt.Source.YamlSource
import Eucalypt.Core.Syn
import Test.Hspec



main :: IO ()
main = hspec spec



spec :: Spec
spec =
  describe "Parses YAML" $ do
    it "Parses key value" $
      parseYamlData "a: !!int 1234" `shouldReturn`
      CoreBlock
        (CoreList
           [CoreList [CorePrim $ CoreSymbol "a", CorePrim $ CoreInt 1234]])
    it "Parses JSON data" $
      parseYamlData " { a: [1, 2, 3], b: {x: \"y\"} } " `shouldReturn`
      CoreBlock
        (CoreList
           [ CoreList
               [ CorePrim $ CoreSymbol "a"
               , CoreList
                   [ CorePrim $ CoreString "1"
                   , CorePrim $ CoreString "2"
                   , CorePrim $ CoreString "3"
                   ]
               ]
           , CoreList
               [ CorePrim $ CoreSymbol "b"
               , CoreBlock
                   (CoreList
                      [ CoreList
                          [CorePrim $ CoreSymbol "x", CorePrim $ CoreString "y"]
                      ])
               ]
           ])
