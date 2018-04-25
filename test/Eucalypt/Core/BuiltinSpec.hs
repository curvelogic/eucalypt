module Eucalypt.Core.BuiltinSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Builtin
import Eucalypt.Core.Syn
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.EvalByName
import Test.Hspec

main :: IO ()
main = hspec spec


-- | Evaluate a zero arity builtin
shallowEvalBuiltin :: CoreBuiltinName -> Either EvaluationError CoreExpr
shallowEvalBuiltin name =
  runInterpreter
    (case lookupBuiltin name of
       Just (_, f) -> f return []
       Nothing -> throwEvalError (BuiltinNotFound name (bif ("__" ++ name))))


spec :: Spec
spec = do
  describe "Builtin lookup" $ do
    it "has null" $
      shallowEvalBuiltin "NULL" `shouldBe` Right corenull
    it "has true" $
      shallowEvalBuiltin "TRUE" `shouldBe` Right (corebool True)
    it "has false" $
      shallowEvalBuiltin "FALSE" `shouldBe` Right (corebool False)
    it "fails sensibly" $
      shallowEvalBuiltin "NONESUCH" `shouldBe`
      Left (BuiltinNotFound "NONESUCH" (bif "__NONESUCH"))
  describe "Boolean operators" $ do -- TODO: quickcheck properties
    it "calculates and" $
      runInterpreter (euAnd return [corebool True, corebool False]) `shouldBe`
      Right (corebool False)
    it "calculates or" $
      runInterpreter (euOr return [corebool True, corebool False]) `shouldBe`
      Right (corebool True)
    it "calculates not(true)" $
      runInterpreter (euNot return [corebool True]) `shouldBe`
      Right (corebool False)
    it "calculates not(false)" $
      runInterpreter (euNot return [corebool False]) `shouldBe`
      Right (corebool True)
  describe "If builtin" $ do
    it "evaluates false branch lazily" $
      runInterpreter (euIf whnfM [corebool True, bif "TRUE", bif "FALSE"]) `shouldBe`
      Right (bif "TRUE")
    it "evaluates true branch lazily" $
      runInterpreter (euIf whnfM [corebool False, bif "TRUE", bif "FALSE"]) `shouldBe`
      Right (bif "FALSE")
  describe "List builtins" $ do
    it "extracts head" $
      runInterpreter (euHead whnfM [CoreList [bif "TRUE", bif "FALSE", bif "FALSE"]]) `shouldBe`
        Right (bif "TRUE")
    it "extracts tail" $
      runInterpreter (euTail whnfM [CoreList [bif "TRUE", bif "FALSE", bif "FALSE"]]) `shouldBe`
        Right (CoreList [bif "FALSE", bif "FALSE"])
    it "throws when head-ing []" $
      runInterpreter (euHead whnfM [CoreList []]) `shouldBe`
        Left (EmptyList (CoreList []))
    it "throws when tail-ing []" $
      runInterpreter (euTail whnfM [CoreList []]) `shouldBe`
        Left (EmptyList (CoreList []))
    it "conses" $
      runInterpreter (euCons whnfM [int 0, CoreList [int 1, int 2, int 3]]) `shouldBe`
      Right (CoreList [int 0, int 1, int 2, int 3])
    it "conses with empty" $
      runInterpreter (euCons whnfM [int 0, CoreList []]) `shouldBe`
      Right (CoreList [int 0])
    it "throws consing a non-list tail" $
      runInterpreter (euCons whnfM [int 0, int 1]) `shouldBe`
      Left (NotList (int 1))
    it "conses with list-valued call" $
      runInterpreter (euCons whnfM [int 0, CoreApp (bif "TAIL") (CoreList [int 1, int 2, int 3])]) `shouldBe`
      Right (CoreList [int 0, int 2, int 3])
