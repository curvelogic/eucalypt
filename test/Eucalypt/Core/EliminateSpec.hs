module Eucalypt.Core.EliminateSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Eliminate
import Eucalypt.Core.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

t1 :: CoreExpr
t1 =
  letexp
    [("a", corenull), ("b", corenull), ("c", var "BOMB")]
    (app (bif "BLAH") [var "a", var "b"])

t1equiv :: CoreExpr
t1equiv =
  letexp
    [("a", corenull), ("b", corenull), ("c", corenull)]
    (app (bif "BLAH") [var "a", var "b"])

t2 :: CoreExpr
t2 =
  letexp
    [ ("a", letexp [("d", var "BOMB"), ("e", corenull)] (var "e"))
    , ("b", var "BOMB")
    ] $
  var "a"

t2equiv :: CoreExpr
t2equiv =
  letexp
    [ ("a", letexp [("d", corenull), ("e", corenull)] (var "e"))
    , ("b", corenull)
    ] $
  var "a"

t3 :: CoreExpr
t3 = letexp [("d", var "BOMB"), ("e", corenull)] (var "e")

t3equiv :: CoreExpr
t3equiv = letexp [("d", corenull), ("e", corenull)] (var "e")

spec :: Spec
spec =
  describe "dead code elimination" $ do
    it "blanks unused bindings in let expressions" $ prune t1 `shouldBe` t1equiv
    it "blanks unused bindings in let expressions" $ prune t3 `shouldBe` t3equiv
    it "blanks unused bindings in nested let expressions" $
      prune t2 `shouldBe` t2equiv
