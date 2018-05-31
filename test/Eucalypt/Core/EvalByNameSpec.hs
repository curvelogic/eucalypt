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
    let i = lam ["x"] (CoreVar "x")
    let k = lam ["x", "y"] (CoreVar "x")
    let s =
          lam
            ["f", "g", "x"]
            (app
               (app (CoreVar "f") [CoreVar "x"])
               [app (CoreVar "g") [CoreVar "x"]])
    it "evals I" $ whnfM (app i [sym "z"]) `shouldBe` return (sym "z")
    it "evals K" $ whnfM (app k [sym "a", str "b"]) `shouldBe` return (sym "a")
    it "evals S" $
      whnfM (app s [k, k, CoreVar "x"]) `shouldBe` whnfM (app i [CoreVar "x"])
    it "evals __NULL" $
      runInterpreter (whnfM (bif "NULL")) `shouldBe` Right corenull
    it "evals __TRUE" $
      runInterpreter (whnfM (bif "TRUE")) `shouldBe` (Right $ corebool True)
    it "evals __FALSE" $
      runInterpreter (whnfM (bif "FALSE")) `shouldBe` (Right $ corebool False)
    it "partially evaluates" $
      runInterpreter (whnfM (app (bif "IF") [corebool False])) `shouldBe`
      Right (CorePAp 3 (bif "IF") [corebool False])
    it "evaluates arity 0 builtins" $
      runInterpreter (whnfM (bif "NULL")) `shouldBe` return corenull
    samplesSpec

samplesSpec :: Spec
samplesSpec =
  describe "head of cons of head / tail" $
  it "evaluates" $
  whnfM
    (app
       (CoreBuiltin "HEAD")
       [ app
           (CoreBuiltin "CONS")
           [ app
               catOp
               [ CoreList
                   [ int 1
                   , int 2
                   , int 3
                   ]
               , CoreBuiltin "HEAD"
               ]
           , app
               catOp
               [ CoreList
                   [ int 1
                   , int 2
                   , int 3
                   ]
               , CoreBuiltin "TAIL"
               ]
           ]
       ]) `shouldBe`
  return (int 1)
