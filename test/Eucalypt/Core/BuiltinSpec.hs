module Eucalypt.Core.BuiltinSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Builtin
import Eucalypt.Core.Syn
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
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
spec =
  describe "Builtin lookup" $ do
    it "has null" $
      shallowEvalBuiltin "NULL" `shouldBe` Right (CorePrim CoreNull)
    it "has true" $
      shallowEvalBuiltin "TRUE" `shouldBe` Right (CorePrim (CoreBoolean True))
    it "has false" $
      shallowEvalBuiltin "FALSE" `shouldBe` Right (CorePrim (CoreBoolean False))
    it "fails sensibly" $
      shallowEvalBuiltin "NONESUCH" `shouldBe` Left (BuiltinNotFound "NONESUCH" (CoreBuiltin "__NONESUCH"))
