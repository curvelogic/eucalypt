module Eucalypt.Core.VerifySpec
  ( main
  , spec
  ) where

import Eucalypt.Core.AnonSyn
import Eucalypt.Core.Verify
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Soup checks" $ do
    it "discovers soup" $
      runChecks (lam ["x", "y"] $ soup [var "x", var "+", var "y"])
      `shouldSatisfy`
      (not . null)
    it "passes fully cooked expressions" $
      runChecks (lam ["x", "y"] $ app (bif "ADD") [var "x", var "y"])
      `shouldSatisfy`
      null
  describe "Closed checks" $ do
    it "discovers free vars" $
      runChecks (lam ["x", "y", "+"] $ soup [var "x", var "+", var "s"])
      `shouldSatisfy`
       (not . null)
    it "passes closed expressions" $
      runChecks (lam ["x", "y", "+"] $ app (bif "ADD") [var "x", var "y"])
      `shouldSatisfy`
       null
