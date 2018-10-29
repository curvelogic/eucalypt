module Eucalypt.Core.EliminateSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Eliminate
import Eucalypt.Core.Syn (CoreExpr, CoreExp(CoreEliminated))
import Eucalypt.Core.AnonSyn
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
    [("a", corenull), ("b", corenull), ("c", CoreEliminated)]
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
    [ ("a", letexp [("d", CoreEliminated), ("e", corenull)] (var "e"))
    , ("b", CoreEliminated)
    ] $
  var "a"

t3 :: CoreExpr
t3 = letexp [("d", var "BOMB"), ("e", corenull)] (var "e")

t3equiv :: CoreExpr
t3equiv = letexp [("d", CoreEliminated), ("e", corenull)] (var "e")


t4 :: CoreExpr
t4 =
  letexp
    [ ("b0", var "x")
    , ("b1", CoreEliminated)
    , ("b2", var "x")
    , ("b3", CoreEliminated)
    ] $
  corelist [var "b0", var "b2"]

t4equiv :: CoreExpr
t4equiv =
  letexp
    [ ("b0", var "x")
    , ("b2", var "x")
    ] $
  corelist [var "b0", var "b2"]


spec :: Spec
spec = do
  describe "dead code elimination" $ do
    it "blanks unused bindings in let expressions" $ prune t1 `shouldBe` t1equiv
    it "blanks unused bindings in let expressions" $ prune t3 `shouldBe` t3equiv
    it "blanks unused bindings in nested let expressions" $
      prune t2 `shouldBe` t2equiv
  describe "bind index adjustment" $
    it "renumbers correctly" $
    newBindIndexes [True, False, False, True, True, False] `shouldBe`
    [Just 0, Nothing, Nothing, Just 1, Just 2, Nothing]
  describe "compresses bindings once eliminated" $
    it "compresses correctly" $
    compress t4 `shouldBe` t4equiv
