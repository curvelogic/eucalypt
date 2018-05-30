module Eucalypt.Core.PrettySpec (main, spec)
where

import Eucalypt.Core.Syn
import Eucalypt.Core.Pretty
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "PPrint" $ do

    it "prints applications" $
      pprint (appexp (var "+") [int 2, int 5]) `shouldBe` "(($+ 2) 5)"

    it "reconstructs bound names in lambdas" $
      pprint (lam ["foo"] (var "foo")) `shouldBe` "(\\ foo. $foo)"

    it "reconstructs bound names in lets" $
      pprint (letexp [("foo", int 2), ("bar", int 3)] (appexp (var "+") [var "foo", var "bar"]))
      `shouldBe` "let foo = 2\n    bar = 3\n    in (($+ $foo) $bar)"

    it "reconstructs bound names even for unused bindings in lets" $
      pprint (letexp [("foo", int 2), ("bar", int 3)] (var "foo"))
      `shouldBe` "let foo = 2\n    bar = 3\n    in $foo"
