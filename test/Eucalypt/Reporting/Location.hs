module Eucalypt.Reporting.Location (main, spec)
where

import Eucalypt.Reporting.Location

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "at" $ do
    it "Constructs located expressions" $
      at (pos "x" 1 15) "foo" `shouldBe`
      Located {location = pos "x" 1 15, locatee = "foo"}
    it "Moves located expressions" $
      move (pos "x" 21 15) (at (pos "y" 1 21) "bar") `shouldBe`
      Located {location = pos "x" 21 15, locatee = "bar"}
