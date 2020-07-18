module Eucalypt.Core.InlinerSpec
  ( main
  , spec
  ) where

import Bound.Scope
import Data.List (elemIndex)
import Eucalypt.Core.AnonSyn
import Eucalypt.Core.Inliner
import Eucalypt.Core.Syn (CoreBindingName, CoreExp(..))
import Test.Hspec

main :: IO ()
main = hspec spec



sampleA :: CoreExpr
sampleA =
  letexp
    [("a", bif "A"), ("b", lam ["x"] (var "x"))]
    (app (var "a") [var "x"])



transpositionSample :: Scope Int CoreExp CoreBindingName
transpositionSample =
  abstract (`elemIndex` ["s", "d", "b"]) $
  app (bif "x") [var "s", var "b", var "d"]



singleTransposition :: CoreExpr
singleTransposition =
  lam ["s", "d", "b"] $ app (bif "x") [var "s", var "b", var "d"]



singleTranspositionApplied :: CoreExpr
singleTranspositionApplied =
  letexp
    [("b", lam ["x", "y"] $ app (bif "F") [var "y", var "x"])]
    (app (var "b") [int 1, int 2])



singleTransposedResult :: CoreExpr
singleTransposedResult = tagInlinables $
  letexp [("b", lam ["x", "y"] $ app (bif "F") [var "y", var "x"])] $
  app (bif "F") [int 2, int 1]



sampleB :: CoreExpr
sampleB =
  letexp
    [ ("i", lam ["x"] $ var "x")
    , ("box", block [element "value" $ app (var "i") [var "x"]])
    ] $
  app (var "lookup") [var "box", sym "value"]



sampleBEquiv :: CoreExpr
sampleBEquiv = tagInlinables $
  letexp
    [ ("i", lam ["x"] $ var "x")
    , ("box", block [element "value" $ var "x"])
    ] $
  app (var "lookup") [var "box", sym "value"]



spec :: Spec
spec =
  describe "Inliner" $ do
    it "inlines synonyms" $
      inline sampleA `shouldBe`
      tagInlinables
        (letexp
           [("a", bif "A"), ("b", lam ["x"] (var "x"))]
           (app (bif "A") [var "x"]))
    it "detects transpositions" $
      isTransposition transpositionSample `shouldBe` True
    it "tags transpositions for inline" $
      isInlinable (tagInlinables singleTransposition) `shouldBe` True
    it "inlines transpositions" $
      inline singleTranspositionApplied `shouldBe` singleTransposedResult
    it "inlines sample B" $
      inline sampleB `shouldBe` sampleBEquiv
