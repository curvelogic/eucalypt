module Eucalypt.Reporting.LocationSpec (main, spec)
where

import Eucalypt.Reporting.Location
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "at" $ do
    it "Constructs located expressions" $
      at (pos "x" 1 15, pos "x" 2 2) "foo" `shouldBe`
      Located {location = (pos "x" 1 15, pos "x" 2 2), locatee = "foo"}
    it "Moves located expressions" $
      move (pos "x" 21 15, pos "x" 22 15) (at (pos "y" 1 21, pos "y" 2 2) "bar") `shouldBe`
      Located {location = (pos "x" 21 15, pos "x" 22 15), locatee = "bar"}
