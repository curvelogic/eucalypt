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
       Nothing -> throwEvalError (BuiltinNotFound name (CoreBuiltin ("__" ++ name))))


spec :: Spec
spec = do
  describe "Builtin lookup" $ do
    it "has null" $
      shallowEvalBuiltin "NULL" `shouldBe` Right (CorePrim CoreNull)
    it "has true" $
      shallowEvalBuiltin "TRUE" `shouldBe` Right (CorePrim (CoreBoolean True))
    it "has false" $
      shallowEvalBuiltin "FALSE" `shouldBe` Right (CorePrim (CoreBoolean False))
    it "fails sensibly" $
      shallowEvalBuiltin "NONESUCH" `shouldBe`
      Left (BuiltinNotFound "NONESUCH" (CoreBuiltin "__NONESUCH"))
  describe "Boolean operators" $ do -- TODO: quickcheck properties
    it "calculates and" $
      runInterpreter (euAnd return [CorePrim $ CoreBoolean True, CorePrim $ CoreBoolean False]) `shouldBe`
      Right (CorePrim $ CoreBoolean False)
    it "calculates or" $
      runInterpreter (euOr return [CorePrim $ CoreBoolean True, CorePrim $ CoreBoolean False]) `shouldBe`
      Right (CorePrim $ CoreBoolean True)
    it "calculates not(true)" $
      runInterpreter (euNot return [CorePrim $ CoreBoolean True]) `shouldBe`
      Right (CorePrim $ CoreBoolean False)
    it "calculates not(false)" $
      runInterpreter (euNot return [CorePrim $ CoreBoolean False]) `shouldBe`
      Right (CorePrim $ CoreBoolean True)
  describe "If builtin" $ do
    it "evaluates false branch lazily" $
      runInterpreter (euIf whnfM [CorePrim $ CoreBoolean True, CoreBuiltin "TRUE", CoreBuiltin "FALSE"]) `shouldBe`
      Right (CoreBuiltin "TRUE")
    it "evaluates true branch lazily" $
      runInterpreter (euIf whnfM [CorePrim $ CoreBoolean False, CoreBuiltin "TRUE", CoreBuiltin "FALSE"]) `shouldBe`
      Right (CoreBuiltin "FALSE")
