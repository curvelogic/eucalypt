module Eucalypt.Core.CookSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Cook
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  cookSpec
  sampleSpec

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

pre100 :: CoreExpr
pre100 = prefix_ 100 (CoreBuiltin "PRE100")

pre10 :: CoreExpr
pre10 = prefix_ 10 (CoreBuiltin "PRE10")

post100 :: CoreExpr
post100 = postfix_ 100 (CoreBuiltin "POST100")

post10 :: CoreExpr
post10 = postfix_ 10 (CoreBuiltin "POST10")

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
    it "handles unary prefix" $
      cookUp [pre100, int 10] `shouldBe`
      (Right $ app (CoreBuiltin "PRE100") [int 10])
    it "handles unary postfix" $
      cookUp [int 10, post100] `shouldBe`
      (Right $ app (CoreBuiltin "POST100") [int 10])
    it "handles mixed high prec. unary & binary" $
      cookUp [pre100, int 20, l50, int 30, post100] `shouldBe`
      (Right $
       app
         (CoreBuiltin "L50")
         [ app (CoreBuiltin "PRE100") [int 20]
         , app (CoreBuiltin "POST100") [int 30]
         ])
    it "handles mixed high prec. unary & binary" $
      cookUp [int 30, post100, l50, pre100, int 20] `shouldBe`
      (Right $
       app
         (CoreBuiltin "L50")
         [ app (CoreBuiltin "POST100") [int 30]
         , app (CoreBuiltin "PRE100") [int 20]
         ])
    it "handles mixed low prec. unary & binary" $
      cookUp [pre10, int 20, l50, int 30, post10] `shouldBe`
      (Right $
       app
         (CoreBuiltin "PRE10")
         [app (CoreBuiltin "POST10") [app (CoreBuiltin "L50") [int 20, int 30]]])
    it "fills section (`l50` 20) with anaphoric var" $
      cookUp [l50, int 20] `shouldBe`
      (Right $ app (CoreBuiltin "L50") [var "_", int 20])
    it "fills section (20 `l50`) with anaphoric var" $
      cookUp [int 20, l50] `shouldBe`
      (Right $ app (CoreBuiltin "L50") [int 20, var "_"])
    it "rejects ... (unary pre) (binary)..." $
      cookUp [pre10, l50, int 30, post10] `shouldBe`
      (Right $
       app
         (bif "PRE10")
         [app (bif "POST10") [app (bif "L50") [var "_", CorePrim (CoreInt 30)]]])
    it "fills ... (binary) (unary post) ... with anaphor" $
      cookUp [int 30, l50, post100, pre100, int 20] `shouldBe`
      (Right $
       app
         (bif "CAT")
         [ app (bif "L50") [int 30, app (bif "POST100") [var "_"]]
         , app (bif "PRE100") [int 20]
         ])
    it "substitutes anaphoric param for  empty" $
      cookUp [] `shouldBe` (Right $ var "_")
    it "corrects pre10 pre10 pre10 pre10 with anaphor" $
      cookUp [pre10, pre10, pre10, pre10] `shouldBe`
      (Right $
       app
         (bif "PRE10")
         [app (bif "PRE10") [app (bif "PRE10") [app (bif "PRE10") [var "_"]]]])
    it "rejects pre10 l50 post10" $
      cookUp [pre10, l50, post10] `shouldBe`
      (Right $
       app
         (bif "PRE10")
         [app (bif "POST10") [app (bif "L50") [var "_", var "_"]]])


sampleA :: [CoreExpr]
sampleA =
  [ bif "HEAD"
  , callOp
  , args
      [ soup
          [ bif "CONS"
          , callOp
          , args
              [ soup [CoreList [int 1, int 2, int 3], bif "HEAD"]
              , soup [CoreList [int 1, int 2, int 3], bif "TAIL"]
              ]
          ]
      ]
  ]

sampleSpec :: Spec
sampleSpec =
  describe "samples" $ do
    it "cooks __HEAD(__CONS([1, 2, 3] __HEAD, [1, 2, 3] __TAIL))" $
      cookUp sampleA `shouldBe`
      (Right $
       app
         (bif "HEAD")
         [ app
             (bif "CONS")
             [ app (bif "CAT") [CoreList [int 1, int 2, int 3], bif "HEAD"]
             , app (bif "CAT") [CoreList [int 1, int 2, int 3], bif "TAIL"]
             ]
         ])
    it "cooks cons(h, t) head" $
      cookUp [bif "CONS", callOp, args [var "h", var "t"], bif "HEAD"] `shouldBe`
      (Right $ app (bif "CAT") [app (bif "CONS") [var "h", var "t"], bif "HEAD"])
    it "cooks x - 1" $
      cookUp [var "x", infixl_ 10 (bif "SUB"), int 1] `shouldBe`
      (Right $ app (bif "SUB") [var "x", int 1])
