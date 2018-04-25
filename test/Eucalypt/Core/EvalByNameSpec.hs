module Eucalypt.Core.EvalByNameSpec (main, spec)
  where

import Test.Hspec
import Eucalypt.Core.Syn
import Eucalypt.Core.EvalByName
import Eucalypt.Core.Interpreter

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "WHNF evaluation" $ do
    let i = lamexp "x" (CoreVar "x")
    let k = lamexpr ["x", "y"] (CoreVar "x")
    let s =
          lamexpr
            ["f", "g", "x"]
            (CoreApp
               (CoreApp (CoreVar "f") (CoreVar "x"))
               (CoreApp (CoreVar "g") (CoreVar "x")))
    it "evals I" $ whnfM (CoreApp i (sym "z")) `shouldBe` return (sym "z")
    it "evals K" $
      whnfM (appexp k [sym "a", str "b"]) `shouldBe` return (sym "a")
    it "evals S" $
      whnfM (CoreApp (CoreApp (CoreApp s k) k) (CoreVar "x")) `shouldBe`
      whnfM (CoreApp i (CoreVar "x"))
    it "evals __NULL" $
      runInterpreter (whnfM (bif "NULL")) `shouldBe` Right corenull
    it "evals __TRUE" $
      runInterpreter (whnfM (bif "TRUE")) `shouldBe` (Right $ corebool True)
    it "evals __FALSE" $
      runInterpreter (whnfM (bif "FALSE")) `shouldBe` (Right $ corebool False)
    it "partially evaluates" $
      runInterpreter (whnfM (CoreApp (bif "IF") (corebool False))) `shouldBe`
      Right (CorePAp 3 (bif "IF") [corebool False])
