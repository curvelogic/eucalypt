module Eucalypt.Core.CookSpec (main, spec)
  where

import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Cook
import Eucalypt.Core.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  cookSpec

l50 :: CoreExpr
l50 = infixl_ 50 (CoreBuiltin "L50")

r50 :: CoreExpr
r50 = infixr_ 50 (CoreBuiltin "R50")

l60 :: CoreExpr
l60 = infixl_ 60 (CoreBuiltin "L60")

r60 :: CoreExpr
r60 = infixr_ 60 (CoreBuiltin "R60")

l40 :: CoreExpr
l40 = infixl_ 40 (CoreBuiltin "L40")

r40 :: CoreExpr
r40 = infixr_ 40 (CoreBuiltin "R40")

cookUp :: [CoreExpr] -> Either EvaluationError CoreExpr
cookUp es = runInterpreter $ cook es

cookSpec :: Spec
cookSpec =
  describe "cooking op soup" $ do
    it "handles 5 + 7" $
      cookUp [int 5, l50, int 7] `shouldBe`
      (Right $ app (CoreBuiltin "L50") [int 5, int 7])
    it "handles catenation" $
      cookUp [var "x", var "f"] `shouldBe`
      (Right $ app (CoreBuiltin "CAT") [var "x", var "f"])
    it "associates left correctly" $
      cookUp [int 1, l50, int 2, l50, int 3] `shouldBe`
      (Right $
       app (CoreBuiltin "L50") [app (CoreBuiltin "L50") [int 1, int 2], int 3])
    it "associates right correctly" $
      cookUp [int 1, r50, int 2, r50, int 3] `shouldBe`
      (Right $
       app (CoreBuiltin "R50") [int 1, app (CoreBuiltin "R50") [int 2, int 3]])
    it "respects precedence in lefts" $
      cookUp [int 1, l40, int 2, l50, int 3, l60, int 4] `shouldBe`
      (Right $
       app
         (CoreBuiltin "L40")
         [ int 1
         , app
             (CoreBuiltin "L50")
             [int 2, app (CoreBuiltin "L60") [int 3, int 4]]
         ])
    it "respects precedence in rights" $
      cookUp [int 1, r60, int 2, r50, int 3, r40, int 4] `shouldBe`
      (Right $
       app
         (CoreBuiltin "R40")
         [ app
             (CoreBuiltin "R50")
             [app (CoreBuiltin "R60") [int 1, int 2], int 3]
         , int 4
         ])
    it "handles unary prefix (!)" pending
    it "handles unary postfix (₌)" pending
    it "rejects ...!+..." pending
    it "rejects ...+?..." pending
    it "rejects empty" pending
    it "cooks recursively?" pending
    it "handles anaphora" pending
    it "symbolises names?" pending
