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
body = appexp (var "+") [var "a", var "b"]

let1 :: CoreExpr
let1 = letexp [("a", int 5)] body

let2 :: CoreExpr
let2 = letexp [("a", int 5), ("b", int 2)] body


letBody :: CoreExpr -> Maybe (Scope (Name String Int) CoreExp CoreBindingName)
letBody (CoreLet _ b) = Just b
letBody _ = Nothing

spec :: Spec
spec =

  describe "abstracting incrementally" $
    it "is equivalent to abstracting all at once" $
      bindMore (\a -> if a == "b" then Just 1 else Nothing) (fromJust $ letBody let1)
      `shouldBe`
      fromJust (letBody let2)
