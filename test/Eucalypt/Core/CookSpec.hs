module Eucalypt.Core.CookSpec
  ( main
  , spec
  ) where

import Data.Either (fromRight)
import Eucalypt.Core.Cook
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

right :: Either l r -> r
right = fromRight undefined

spec :: Spec
spec = do
  distributeFixitySpec
  cookSpec
  sampleSpec

l50 :: CoreExpr
l50 = infixl_ 50 (bif "L50")

r50 :: CoreExpr
r50 = infixr_ 50 (bif "R50")

l60 :: CoreExpr
l60 = infixl_ 60 (bif "L60")

r60 :: CoreExpr
r60 = infixr_ 60 (bif "R60")

l40 :: CoreExpr
l40 = infixl_ 40 (bif "L40")

r40 :: CoreExpr
r40 = infixr_ 40 (bif "R40")

pre100 :: CoreExpr
pre100 = prefix_ 100 (bif "PRE100")

pre10 :: CoreExpr
pre10 = prefix_ 10 (bif "PRE10")

post100 :: CoreExpr
post100 = postfix_ 100 (bif "POST100")

post10 :: CoreExpr
post10 = postfix_ 10 (bif "POST10")

cookUp :: [CoreExpr] -> CoreExpr
cookUp es = right $ runInterpreter $ cook es

cookSpec :: Spec
cookSpec =
  describe "cooking op soup" $ do
    it "handles 5 + 7" $
      cookUp [int 5, l50, int 7] `shouldBe` app (bif "L50") [int 5, int 7]
    it "handles catenation" $
      cookUp [var "x", var "f"] `shouldBe` app (bif "CAT") [var "x", var "f"]
    it "associates left correctly" $
      cookUp [int 1, l50, int 2, l50, int 3] `shouldBe`
      app (bif "L50") [app (bif "L50") [int 1, int 2], int 3]
    it "associates right correctly" $
      cookUp [int 1, r50, int 2, r50, int 3] `shouldBe`
      app (bif "R50") [int 1, app (bif "R50") [int 2, int 3]]
    it "respects precedence in lefts" $
      cookUp [int 1, l40, int 2, l50, int 3, l60, int 4] `shouldBe`
      app
        (bif "L40")
        [int 1, app (bif "L50") [int 2, app (bif "L60") [int 3, int 4]]]
    it "respects precedence in rights" $
      cookUp [int 1, r60, int 2, r50, int 3, r40, int 4] `shouldBe`
      app
        (bif "R40")
        [app (bif "R50") [app (bif "R60") [int 1, int 2], int 3], int 4]
    it "handles unary prefix" $
      cookUp [pre100, int 10] `shouldBe` app (bif "PRE100") [int 10]
    it "handles unary postfix" $
      cookUp [int 10, post100] `shouldBe` app (bif "POST100") [int 10]
    it "handles mixed high prec. unary & binary" $
      cookUp [pre100, int 20, l50, int 30, post100] `shouldBe`
      app
        (bif "L50")
        [app (bif "PRE100") [int 20], app (bif "POST100") [int 30]]
    it "handles mixed high prec. unary & binary" $
      cookUp [int 30, post100, l50, pre100, int 20] `shouldBe`
      app
        (bif "L50")
        [app (bif "POST100") [int 30], app (bif "PRE100") [int 20]]
    it "handles mixed low prec. unary & binary" $
      cookUp [pre10, int 20, l50, int 30, post10] `shouldBe`
      app (bif "PRE10") [app (bif "POST10") [app (bif "L50") [int 20, int 30]]]
    it "fills section (`l50` 20) with anaphoric var and abstracts" $
      cookUp [l50, int 20] `shouldBe`
      lam ["_0"] (app (bif "L50") [var "_0", int 20])
    it "fills section (20 `l50`) with anaphoric var and abstracts" $
      cookUp [int 20, l50] `shouldBe`
      lam ["_0"] (app (bif "L50") [int 20, var "_0"])
    it "fills ... (unary pre) (binary)... and abstracts" $
      cookUp [pre10, l50, int 30, post10] `shouldBe`
      lam
        ["_0"]
        (app
           (bif "PRE10")
           [ app
               (bif "POST10")
               [app (bif "L50") [var "_0", CorePrim (CoreInt 30)]]
           ])
    it "fills ... (binary) (unary post) ... with anaphor and abstracts" $
      cookUp [int 30, l50, post100, pre100, int 20] `shouldBe`
      lam
        ["_0"]
        (app
           (bif "CAT")
           [ app (bif "L50") [int 30, app (bif "POST100") [var "_0"]]
           , app (bif "PRE100") [int 20]
           ])
    it "substitutes anaphoric lambda for empty" $
      cookUp [] `shouldBe` lam ["_0"] (var "_0")
    it "corrects pre10 pre10 pre10 pre10 with anaphor and abstracts" $
      cookUp [pre10, pre10, pre10, pre10] `shouldBe`
      lam
        ["_0"]
        (app
           (bif "PRE10")
           [ app
               (bif "PRE10")
               [app (bif "PRE10") [app (bif "PRE10") [var "_0"]]]
           ])
    it "rejects pre10 l50 post10" $
      cookUp [pre10, l50, post10] `shouldBe`
      lam
        ["_0", "_1"]
        (app
           (bif "PRE10")
           [app (bif "POST10") [app (bif "L50") [var "_0", var "_1"]]])


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
      app
         (bif "HEAD")
         [ app
             (bif "CONS")
             [ app (bif "CAT") [CoreList [int 1, int 2, int 3], bif "HEAD"]
             , app (bif "CAT") [CoreList [int 1, int 2, int 3], bif "TAIL"]
             ]
         ]
    it "cooks cons(h, t) head" $
      cookUp [bif "CONS", callOp, args [var "h", var "t"], bif "HEAD"] `shouldBe`
      app (bif "CAT") [app (bif "CONS") [var "h", var "t"], bif "HEAD"]
    it "cooks x - 1" $
      cookUp [var "x", infixl_ 10 (bif "SUB"), int 1] `shouldBe`
      app (bif "SUB") [var "x", int 1]


distributeSample1 :: CoreExpr
distributeSample1 =
  letexp [("+", infixl_ 50 (bif "FOO")), ("-", infixl_ 50 (bif "BAR"))] $
  soup [int 1, var "+", int 2]

distributeResult1 :: CoreExpr
distributeResult1 =
  letexp [("+", bif "FOO"), ("-", bif "BAR")] $
  soup [int 1, infixl_ 50 $ var "+", int 2]

distributeSample2 :: CoreExpr
distributeSample2 =
  letexp [("+", infixl_ 50 (bif "FOO"))] $
  letexp [("-", infixl_ 50 (bif "BAR"))] $
  soup [int 1, var "+", int 2]

distributeResult2 :: CoreExpr
distributeResult2 =
  letexp [("+", bif "FOO")] $
  letexp [("-", bif "BAR")] $
  soup [int 1, infixl_ 50 $ var "+", int 2]

distributeSample3 :: CoreExpr
distributeSample3 =
  letexp [("+", infixl_ 50 (bif "FOO"))] $
  letexp [("-", infixr_ 90 (bif "BAR"))] $
  soup [int 1, var "-", int 2]

distributeResult3 :: CoreExpr
distributeResult3 =
  letexp [("+", bif "FOO")] $
  letexp [("-", bif "BAR")] $
  soup [int 1, infixr_ 90 $ var "-", int 2]

distributeFixitySpec :: Spec
distributeFixitySpec =
  describe "fixity distribution" $ do
    it "distributes with single level of let" $
      distributeFixities distributeSample1 `shouldBe` distributeResult1
    it "distributes through intermediate lets" $
      distributeFixities distributeSample2 `shouldBe` distributeResult2
    it "distributes within intermediate lets" $
      distributeFixities distributeSample3 `shouldBe` distributeResult3
