{-|
Module      : Eucalypt.Core.BlockAnaphoraSpec
Description : Test for block anaphora processing
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.BlockAnaphoraSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.BlockAnaphora
import Eucalypt.Core.AnonSyn
import Test.Hspec

main :: IO ()
main = hspec spec

sampleA :: CoreExpr
sampleA =
  letexp [("x", var "•"), ("y", var "•")] $
  block [element "x" $ var "x", element "y" $ var "y"]

sampleAEquiv :: CoreExpr
sampleAEquiv =
  lam ["•0", "•1"] $
  letexp [("x", var "•0"), ("y", var "•1")] $
  block [element "x" $ var "x", element "y" $ var "y"]


sampleB :: CoreExpr
sampleB =
  letexp
    [ ("a", letexp [("x", var "•0")] $ block [element "x" $ var "x"])
    , ("b", app (var"a") [int 1])
    ] $
  block [element "a" $ var "a", element "b" $ var "b"]

sampleBEquiv :: CoreExpr
sampleBEquiv =
  letexp
    [ ("a", lam ["•0"] $ letexp [("x", var "•0")] $ block [element "x" $ var "x"])
    , ("b", app (var "a") [int 1])
    ] $
  block [element "a" $ var "a", element "b" $ var "b"]

spec :: Spec
spec = do
  describe "detects block anaphora" $ do
    it "detects in op soup" $
      hasNakedBlockAnaphora (soup [var "x", var "•", var "y"]) `shouldBe` True
    it "detects in list" $
      hasNakedBlockAnaphora (corelist [var "x", var "•", var "y"]) `shouldBe`
      True
    it "ignores instances in blocks" $
      hasNakedBlockAnaphora (block [element "x" $ var "•"]) `shouldBe` False
    it "ignores instances in lets" $
      hasNakedBlockAnaphora (letexp [("x", var "•")] $ var "x") `shouldBe` False
  describe "expands anaphoric blocks into lambdas" $ do
    it "expands { x: • y: • }" $ anaphorise sampleA `shouldBe` sampleAEquiv
    it "expands { a: { x: •0 } b: a(1) }" $
      anaphorise sampleB `shouldBe` sampleBEquiv
