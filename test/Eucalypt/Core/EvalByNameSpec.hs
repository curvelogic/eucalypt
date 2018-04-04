module Eucalypt.Core.EvalByNameSpec (main, spec)
  where

import Test.Hspec
import Eucalypt.Core.Syn
import Eucalypt.Core.EvalByName

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "WHNF evaluation" $ do

    let i = (lamexp "x" (CoreVar "x"))
    let k = (lamexpr ["x", "y"] (CoreVar "x"))
    let s = (lamexpr ["f", "g", "x"] (CoreApp
                                       (CoreApp (CoreVar "f") (CoreVar "x"))
                                       (CoreApp (CoreVar "g") (CoreVar "x"))))

    it "evals I" $
      whnf (CoreApp i (CorePrim (Symbol "z"))) `shouldBe` CorePrim (Symbol "z")

    it "evals K" $
      whnf (appexp k [CorePrim (Symbol "a"), CorePrim (String "b")])
      `shouldBe`
      CorePrim (Symbol "a")

    it "evals S" $
      whnf (CoreApp (CoreApp (CoreApp s k) k) (CoreVar "x"))
      `shouldBe`
      whnf (CoreApp i (CoreVar "x"))

