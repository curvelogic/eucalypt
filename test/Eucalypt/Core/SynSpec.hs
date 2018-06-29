module Eucalypt.Core.SynSpec
  ( main
  , spec
  ) where

import Bound
import Bound.Name
import Eucalypt.Core.Syn
import Test.Hspec
import Data.Maybe (fromJust)

main :: IO ()
main = hspec spec


body :: CoreExpr
body = app (var "+") [var "a", var "b"]

let1 :: CoreExpr
let1 = letexp [("a", int 5)] body

let2 :: CoreExpr
let2 = letexp [("a", int 5), ("b", int 2)] body


letBody :: CoreExpr -> Maybe (Scope (Name String Int) CoreExp CoreBindingName)
letBody (CoreLet _ b) = Just b
letBody _ = Nothing

bodyA :: CoreExpr
bodyA = block [element "a1" (var "a1"), element "a2" (var "a2")]

bodyA2 :: CoreExpr
bodyA2 = block [element "a1froma" (var "a1")]

bodyA3 :: CoreExpr
bodyA3 = block [element "a2froma" (var "a2")]

unitA :: CoreExpr
unitA = letexp [("a1", int 5), ("a2", int 6)] bodyA

unitA2 :: CoreExpr
unitA2 = letexp [("a1", int 5), ("a2", int 6)] bodyA2

unitA3 :: CoreExpr
unitA3 = letexp [("a1", int 5), ("a2", var "a1")] bodyA3

bodyB :: CoreExpr
bodyB = block [element "b1" (var "b1"), element "b2" (var "b2")]

bodyB2 :: CoreExpr
bodyB2 =
  block
    [ element "b1fromb" (var "b1")
    , element "a1fromb" (var "a1")
    ]

bodyB3 :: CoreExpr
bodyB3 =
  block
    [ element "b2fromb" (var "b2")
    , element "a2fromb" (var "a2")
    ]

unitB :: CoreExpr
unitB = letexp [("b1", int 5), ("b2", int 6)] bodyB

unitB2 :: CoreExpr
unitB2 = letexp [("b1", int 5), ("b2", int 6)] bodyB2

unitB3 :: CoreExpr
unitB3 = letexp [("b1", int 5), ("b2", var "b1")] bodyB3

bodyC :: CoreExpr
bodyC = block [element "c1" (var "c1"), element "c2" (var "c2")]

bodyC2 :: CoreExpr
bodyC2 =
  block
    [ element "c1fromc" (var "c1")
    , element "b1fromc" (var "b1")
    , element "a1fromc" (var "a1")
    ]

bodyC3 :: CoreExpr
bodyC3 =
  block
    [ element "c2fromc" (var "c2")
    , element "b2fromc" (var "b2")
    , element "a2fromc" (var "a2")
    ]

unitC :: CoreExpr
unitC = letexp [("c1", int 5), ("c2", int 6)] bodyC

unitC2 :: CoreExpr
unitC2 = letexp [("c1", int 5), ("c2", int 6)] bodyC2

unitC3 :: CoreExpr
unitC3 = letexp [("c1", int 5), ("c2", var "c1")] bodyC3

spec :: Spec
spec = do

  describe "abstracting incrementally" $
    it "is equivalent to abstracting all at once" $
      bindMore (\a -> if a == "b" then Just 1 else Nothing) (fromJust $ letBody let1)
      `shouldBe`
      fromJust (letBody let2)
  mergeUnitsSpec
  anaphoraSpec
  evaluandSpec

anaphoraSpec :: Spec
anaphoraSpec = do
  describe "expression anaphora" $ do
    it "_x is not anaphoric" $ isAnaphoricVar (var "_x") `shouldBe` False
    it "_3 is anaphoric" $ isAnaphoricVar (var "_3") `shouldBe` True
  describe "anaphora numbering" $
    it "numbers [_, _, _]" $
      numberAnaphora (corelist [var "_", var "_", var "_"]) `shouldBe`
      corelist [var "_0", var "_1", var "_2"]
  describe "binding anaphora" $ do
    it "binds [_0, _1, _2]" $
      bindAnaphora (corelist [var "_0", var "_1", var "_2"]) `shouldBe`
        lam ["_0", "_1", "_2"] (corelist [var "_0", var "_1", var "_2"])
    it "binds [_2]" $
      bindAnaphora (corelist [var "_2"]) `shouldBe`
        lam ["_0", "_1", "_2"] (corelist [var "_2"])


mergeUnitsSpec :: Spec
mergeUnitsSpec =
  describe "merging units" $ do
    xit "merges and binds correctly with no cross unit bindings" $
      mergeUnits [unitA, unitB, unitC] `shouldBe`
      app
        (letexp [("c1", int 5), ("c2", int 6),("b1", int 5), ("b2", int 6),("a1", int 5), ("a2", int 6)] bodyC)
        [app
         (letexp [("b1", int 5), ("b2", int 6),("a1", int 5), ("a2", int 6)] bodyB)
         [letexp [("a1", int 5), ("a2", int 6)] bodyA]]

    xit "merges and binds correctly with cross unit bindings" $
      mergeUnits [unitA2, unitB2, unitC2] `shouldBe`
      app
        (letexp [("c1", int 5), ("c2", int 6),("b1", int 5), ("b2", int 6),("a1", int 5), ("a2", int 6)] bodyC2)
        [app
         (letexp [("b1", int 5), ("b2", int 6),("a1", int 5), ("a2", int 6)] bodyB2)
         [letexp [("a1", int 5), ("a2", int 6)] bodyA2]]

    xit "merges and binds correctly with cross unit bindings and internal references" $
      mergeUnits [unitA3, unitB3, unitC3] `shouldBe`
      app
        (letexp [("c1", int 5), ("c2", var "c1"), ("b1", int 5), ("b2", var "b1"), ("a1", int 5), ("a2", var "a1")] bodyC3)
        [app
         (letexp [("b1", int 5), ("b2", var "b1"),("a1", int 5), ("a2", var "a1")] bodyB3)
         [letexp [("a1", int 5), ("a2", var "a1")] bodyA3]]

doubleLetTarget :: CoreExpr
doubleLetTarget =
  letexp [("three", int 3)] $
  letexp [("two", int 2)] $ letexp [("one", int 1)] (var "one")

doubleLetPayload1 :: CoreExpr
doubleLetPayload1 = var "three"

doubleLetResult1 :: CoreExpr
doubleLetResult1 =
  letexp [("three", int 3)] $
  letexp [("two", int 2)] $ letexp [("one", int 1)] (var "three")


letMetaLetTarget :: CoreExpr
letMetaLetTarget =
  letexp [("three", int 3)] $
  withMeta (block []) $
  letexp [("two", int 2)] $ letexp [("one", int 1)] (var "one")

letMetaLetPayload1 :: CoreExpr
letMetaLetPayload1 = var "three"

letMetaLetResult1 :: CoreExpr
letMetaLetResult1 =
  letexp [("three", int 3)] $
  withMeta (block []) $
  letexp [("two", int 2)] $ letexp [("one", int 1)] (var "three")


letTracedLetTarget :: CoreExpr
letTracedLetTarget =
  letexp [("three", int 3)] $
  CoreTraced $
  letexp [("two", int 2)] $ letexp [("one", int 1)] (var "one")

letTracedLetPayload1 :: CoreExpr
letTracedLetPayload1 = var "three"

letTracedLetResult1 :: CoreExpr
letTracedLetResult1 =
  letexp [("three", int 3)] $
  CoreTraced $
  letexp [("two", int 2)] $ letexp [("one", int 1)] (var "three")


letCheckedLetTarget :: CoreExpr
letCheckedLetTarget =
  letexp [("three", int 3)] $
  CoreChecked (bif "*") $
  letexp [("two", int 2)] $ letexp [("one", int 1)] (var "one")

letCheckedLetPayload1 :: CoreExpr
letCheckedLetPayload1 = var "three"

letCheckedLetResult1 :: CoreExpr
letCheckedLetResult1 =
  letexp [("three", int 3)] $
  letexp [("two", int 2)] $ letexp [("one", int 1)] (var "three")


evaluandSpec :: Spec
evaluandSpec =
  describe "evaluand delivery" $ do
    it "modifies payload in two lets" $
      rebody doubleLetTarget doubleLetPayload1 `shouldBe` doubleLetResult1
    it "modifies payload in lets and metas" $
      rebody letMetaLetTarget letMetaLetPayload1 `shouldBe` letMetaLetResult1
    it "leaves traces in place" $
      rebody letTracedLetTarget letTracedLetPayload1 `shouldBe` letTracedLetResult1
    it "removes checks" $
      rebody letCheckedLetTarget letCheckedLetPayload1 `shouldBe` letCheckedLetResult1
