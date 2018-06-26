module Eucalypt.Core.EvalByNameSpec (main, spec)
  where

import Test.Hspec
import Data.Either (fromRight)
import Eucalypt.Core.Syn
import Eucalypt.Core.EvalByName
import Eucalypt.Core.Interpreter

right :: Either l r -> r
right = fromRight undefined

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "WHNF evaluation" $ do
    let i = lam ["x"] (CoreVar "x")
    let k = lam ["x", "y"] (CoreVar "x")
    let s =
          lam
            ["f", "g", "x"]
            (app
               (app (CoreVar "f") [CoreVar "x"])
               [app (CoreVar "g") [CoreVar "x"]])
    it "evals I" $
      (right . runInterpreter . whnfM) (app i [sym "z"]) `shouldBe` sym "z"
    it "evals K" $
      (right . runInterpreter . whnfM) (app k [sym "a", str "b"]) `shouldBe`
      sym "a"
    it "evals S" $
      (right . runInterpreter . whnfM) (app s [k, k, CoreVar "x"]) `shouldBe`
      (right . runInterpreter . whnfM) (app i [CoreVar "x"])
    it "evals __NULL" $
      (right . runInterpreter) (whnfM (bif "NULL")) `shouldBe` corenull
    it "evals __TRUE" $
      (right . runInterpreter) (whnfM (bif "TRUE")) `shouldBe` corebool True
    it "evals __FALSE" $
      (right . runInterpreter) (whnfM (bif "FALSE")) `shouldBe` corebool False
    it "partially evaluates" $
      (right . runInterpreter) (whnfM (app (bif "IF") [corebool False])) `shouldBe`
      CorePAp 3 (bif "IF") [corebool False]
    it "evaluates arity 0 builtins" $
      (right . runInterpreter) (whnfM (bif "NULL")) `shouldBe` corenull
    samplesSpec

samplesSpec :: Spec
samplesSpec =
  describe "head of cons of head / tail" $
  it "evaluates" $
  (right . runInterpreter . whnfM)
    (app
       (CoreBuiltin "HEAD")
       [ app
           (CoreBuiltin "CONS")
           [ app catOp [CoreList [int 1, int 2, int 3], CoreBuiltin "HEAD"]
           , app catOp [CoreList [int 1, int 2, int 3], CoreBuiltin "TAIL"]
           ]
       ]) `shouldBe`
  int 1
