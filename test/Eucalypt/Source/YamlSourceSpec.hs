{-# LANGUAGE OverloadedStrings #-}

module Eucalypt.Source.YamlSourceSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.AnonSyn
import Eucalypt.Source.YamlSource
import Test.Hspec
import Text.Libyaml (Tag(..))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Yaml parser" $ do
    it "Parses key value" $
      parseYamlExpr "<test>" "a: !!int 1234" `shouldReturn`
      letexp [("a", int 1234)] (block [element "a" $ var "a"])
    it "Parses JSON data" $
      parseYamlExpr "<test>" " { a: [1, 2, 3], b: {x: \"y\"} } " `shouldReturn`
      letexp
        [ ("a", corelist [int 1, int 2, int 3])
        , ("b", letexp [("x", str "y")] $ block [element "x" $ var "x"])
        ]
        (block [element "a" $ var "a", element "b" $ var "b"])
    it "Parses bools" $
      parseYamlExpr
        "<test>"
        "a: true\nb: True\nc: TRUE\nd: false\ne: False\nf: FALSE " `shouldReturn`
      letexp
        [ ("a", corebool True)
        , ("b", corebool True)
        , ("c", corebool True)
        , ("d", corebool False)
        , ("e", corebool False)
        , ("f", corebool False)
        ]
        (block
           [ element "a" $ var "a"
           , element "b" $ var "b"
           , element "c" $ var "c"
           , element "d" $ var "d"
           , element "e" $ var "e"
           , element "f" $ var "f"
           ])
    it "Resolves unknown tags" $ do
      coreTagResolve "null" `shouldBe` NullTag
      coreTagResolve "Null" `shouldBe` NullTag
      coreTagResolve "NULL" `shouldBe` NullTag
      coreTagResolve "nULL" `shouldBe` StrTag
      coreTagResolve "" `shouldBe` NullTag
      coreTagResolve "true" `shouldBe` BoolTag
      coreTagResolve "True" `shouldBe` BoolTag
      coreTagResolve "TRUE" `shouldBe` BoolTag
      coreTagResolve "tRUE" `shouldBe` StrTag
      coreTagResolve "false" `shouldBe` BoolTag
      coreTagResolve "False" `shouldBe` BoolTag
      coreTagResolve "FALSE" `shouldBe` BoolTag
      coreTagResolve "fALSE" `shouldBe` StrTag
      coreTagResolve "0" `shouldBe` IntTag
      coreTagResolve "0o7" `shouldBe` IntTag
      coreTagResolve "0x3A" `shouldBe` IntTag
      coreTagResolve "-19" `shouldBe` IntTag
      coreTagResolve "0." `shouldBe` FloatTag
      coreTagResolve "-0.0" `shouldBe` FloatTag
      coreTagResolve ".5" `shouldBe` FloatTag
      coreTagResolve "+12e03" `shouldBe` FloatTag
      coreTagResolve "-2E+05" `shouldBe` FloatTag
      coreTagResolve ".inf" `shouldBe` FloatTag
      coreTagResolve "-.Inf" `shouldBe` FloatTag
      coreTagResolve "+.INF" `shouldBe` FloatTag
      coreTagResolve ".NAN" `shouldBe` FloatTag
