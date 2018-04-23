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

    let i = lamexp "x" (CoreVar "x")
    let k = lamexpr ["x", "y"] (CoreVar "x")
    let s = lamexpr ["f", "g", "x"] (CoreApp
                                      (CoreApp (CoreVar "f") (CoreVar "x"))
                                      (CoreApp (CoreVar "g") (CoreVar "x")))

    it "evals I" $
      whnfM (CoreApp i (CorePrim (CoreSymbol "z"))) `shouldBe` return (CorePrim (CoreSymbol "z"))

    it "evals K" $
      whnfM (appexp k [CorePrim (CoreSymbol "a"), CorePrim (CoreString "b")])
      `shouldBe`
      return (CorePrim (CoreSymbol "a"))

    it "evals S" $
      whnfM (CoreApp (CoreApp (CoreApp s k) k) (CoreVar "x"))
      `shouldBe`
      whnfM (CoreApp i (CoreVar "x"))
