{-# LANGUAGE LambdaCase #-}

module Eucalypt.Core.DesugarSpec
  ( main
  , spec
  ) where

import Control.Monad.State.Strict
import Eucalypt.Core.Desugar
import Eucalypt.Core.Target
import qualified Eucalypt.Core.Syn as Syn
import Eucalypt.Core.Unit
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.ParseExpr
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  coreSpec
  soupSpec
  blockSpec
  sampleSpec
  targetsSpec
  interpolationSpec

-- ? shims
testDesugarSoup :: [Expression] -> Syn.CoreExpr
testDesugarSoup = (`evalState` initTranslateState) . unTranslate . translateSoup

testDesugarBlock :: Block -> Syn.CoreExpr
testDesugarBlock = (`evalState` initTranslateState) . unTranslate . translateBlock

testDesugar :: Expression -> Syn.CoreExpr
testDesugar = (`evalState` initTranslateState) . unTranslate . translate


coreSpec :: Spec
coreSpec =
  describe "Core" $ do
    it "represents literals" $ desugarLiteral (VInt 8) `shouldBe` Syn.int 8
    it "processes annotation shortcuts" $
      processAnnotation (Syn.CorePrim (Syn.CoreString "blah")) `shouldBe`
      Syn.CoreBlock
        (Syn.CoreList
           [ Syn.CoreList
               [ Syn.CorePrim (Syn.CoreSymbol "doc")
               , Syn.CorePrim (Syn.CoreString "blah")
               ]
           ])
    it "preserves declaration metadata" $
      Syn.unbind
        (testDesugarBlock (at nowhere $ Block [ann (str "docs") (prop "x" (int 5))])) `shouldSatisfy` \case
        Syn.CoreBlock (Syn.CoreList [Syn.CoreMeta _ _]) -> True
        _ -> False


soupSpec :: Spec
soupSpec =
  describe "soup desugaring" $ do
    it "inserts call operators" $
      testDesugarSoup
        [int 5, normalName "x", normalName "f", applyTuple [int 4, int 7]] `shouldBe`
      Syn.soup
        [ Syn.int 5
        , Syn.var "x"
        , Syn.var "f"
        , Syn.callOp
        , Syn.args [Syn.int 4, Syn.int 7]
        ]
    it "handles iterated calls" $
      testDesugarSoup
        [ normalName "f"
        , applyTuple [normalName "x"]
        , applyTuple [normalName "y"]
        ] `shouldBe`
      Syn.soup
        [ Syn.var "f"
        , Syn.callOp
        , Syn.args [Syn.var "x"]
        , Syn.callOp
        , Syn.args [Syn.var "y"]
        ]
    it "handles relative names" $
      testDesugarSoup
        [ normalName "x"
        , operatorName "."
        , normalName "y"
        , operatorName "."
        , normalName "z"
        ] `shouldBe`
      Syn.soup
        [ Syn.var "x"
        , Syn.lookupOp
        , Syn.corename "y"
        , Syn.lookupOp
        , Syn.corename "z"
        ]

blockSpec :: Spec
blockSpec =
  describe "block desugaring" $ do
    it "creates vars for lonely names" $
      testDesugarBlock (at nowhere $ Block [bare (prop "x" (normalName "y"))]) `shouldBe`
      Syn.letexp
        [("x", Syn.var "y")]
        (Syn.block [Syn.element "x" $ Syn.var "x"])
    it "handles built-ins" $
      testDesugarBlock
        (at nowhere $
         Block
           [ bare (prop "null" (normalName "__NULL"))
           , bare (prop "a" (normalName "null"))
           ]) `shouldBe`
      Syn.letexp
        [("null", Syn.bif "NULL"), ("a", Syn.var "null")]
        (Syn.block
           [Syn.element "null" $ Syn.var "null", Syn.element "a" $ Syn.var "a"])

sampleSpec :: Spec
sampleSpec =
  describe "samples" $ do
    it "desugars eq(or(f, and(t, t)), t)" $
      testDesugar <$>
      parseExpression "eq(or(f, and(t, t)), t)" "test" `shouldBe`
      Right
        (Syn.soup
           [ Syn.var "eq"
           , Syn.callOp
           , Syn.args
               [ Syn.soup
                   [ Syn.var "or"
                   , Syn.callOp
                   , Syn.args
                       [ Syn.var "f"
                       , Syn.soup
                           [ Syn.var "and"
                           , Syn.callOp
                           , Syn.args [Syn.var "t", Syn.var "t"]
                           ]
                       ]
                   ]
               , Syn.var "t"
               ]
           ])
    it "desugars __HEAD(__CONS([1, 2, 3] __HEAD, [1, 2, 3] __TAIL))" $
      testDesugar <$>
      parseExpression
        "__HEAD(__CONS([1, 2, 3] __HEAD, [1, 2, 3] __TAIL))"
        "test" `shouldBe`
      Right
        (Syn.soup
           [ Syn.bif "HEAD"
           , Syn.callOp
           , Syn.args
               [ Syn.soup
                   [ Syn.bif "CONS"
                   , Syn.callOp
                   , Syn.args
                       [ Syn.soup
                           [ Syn.CoreList [Syn.int 1, Syn.int 2, Syn.int 3]
                           , Syn.bif "HEAD"
                           ]
                       , Syn.soup
                           [ Syn.CoreList [Syn.int 1, Syn.int 2, Syn.int 3]
                           , Syn.bif "TAIL"
                           ]
                       ]
                   ]
               ]
           ])
    it "desugars x - 1" $
      testDesugar <$>
      parseExpression "x - 1" "test" `shouldBe`
      Right (Syn.soup [Syn.var "x", Syn.var "-", Syn.int 1])


targetAnnotation :: String -> String -> Expression
targetAnnotation n d = block [bare $ prop "target" $ sym n , bare $ prop "doc" $ str d]

targetSampleA :: Expression
targetSampleA =
  block
    [ bare $
      prop "a" $ block [ann (targetAnnotation "T" "x") (prop "b" $ int 1)]
    ]

targetSampleB :: Expression
targetSampleB =
  block
    [ bare $
      prop "a" $
      block
        [ ann (targetAnnotation "T" "x") (prop "b" $ int 1)
        , ann (targetAnnotation "U" "y") (prop "c" $ int 1)
        ]
    ]

targetsSpec :: Spec
targetsSpec =
  describe "target detection" $ do
    it "reads annotation ` {target: :T doc: \"x\"}" $
      (determineTarget . Just . testDesugar) (targetAnnotation "T" "x") `shouldBe`
      Just ("T", "x")
    it "finds T in { a: { ` {target: :T doc: \"x\"} b: _ } }" $
      (truTargets . translateToCore) targetSampleA `shouldBe`
      [TargetSpec "T" "x" ["a", "b"]]
    it "finds T and U in larger sample " $
      (truTargets . translateToCore) targetSampleB `shouldBe`
      [TargetSpec "T" "x" ["a", "b"], TargetSpec "U" "y" ["a", "c"]]


interpolationSpec:: Spec
interpolationSpec =
  describe "string interpolation" $ do
    it "decomposes pattern \"{foo}bar\"" $
      testDesugar <$>
      parseExpression "\"{foo}bar\"" "test" `shouldBe`
      Right
        (Syn.app
           (Syn.bif "JOIN")
           [ Syn.CoreList
               [Syn.app (Syn.bif "STR") [Syn.var "foo"], Syn.str "bar"]
           , Syn.str ""
           ])
    it "decomposes pattern \"{foo}bar\"" $
      testDesugar <$>
      parseExpression "\"foo{bar}\"" "test" `shouldBe`
      Right
        (Syn.app
           (Syn.bif "JOIN")
           [ Syn.CoreList
               [Syn.str "foo", Syn.app (Syn.bif "STR") [Syn.var "bar"]]
           , Syn.str ""
           ])
    it "decomposes pattern \"{foo}{bar}\"" $
      testDesugar <$>
      parseExpression "\"{foo}{bar}\"" "test" `shouldBe`
      Right
        (Syn.app
           (Syn.bif "JOIN")
           [ Syn.CoreList
               [ Syn.app (Syn.bif "STR") [Syn.var "foo"]
               , Syn.app (Syn.bif "STR") [Syn.var "bar"]
               ]
           , Syn.str ""
           ])
    it "handles empty string \"\"" $
      testDesugar <$>
      parseExpression "\"\"" "test" `shouldBe` Right (Syn.str "")
    it "desugars numbered anaphora" $
      testDesugar <$>
      parseExpression "\"{0}{1}\"" "test" `shouldBe`
      Right
        (Syn.lam
           ["_0", "_1"]
           (Syn.app
              (Syn.bif "JOIN")
              [ Syn.CoreList
                  [ Syn.app (Syn.bif "STR") [Syn.var "_0"]
                  , Syn.app (Syn.bif "STR") [Syn.var "_1"]
                  ]
              , Syn.str ""
              ]))
    it "desugars unnumbered anaphora" $
      testDesugar <$>
      parseExpression "\"{}{}\"" "test" `shouldBe`
      Right
        (Syn.lam
           ["_0", "_1"]
           (Syn.app
              (Syn.bif "JOIN")
              [ Syn.CoreList
                  [ Syn.app (Syn.bif "STR") [Syn.var "_0"]
                  , Syn.app (Syn.bif "STR") [Syn.var "_1"]
                  ]
              , Syn.str ""
              ]))
    it "desugars unnumbered anaphora with free vars" $
      testDesugar <$>
      parseExpression "\"{foo}{}\"" "test" `shouldBe`
      Right
        (Syn.lam
           ["_0"]
           (Syn.app
              (Syn.bif "JOIN")
              [ Syn.CoreList
                  [ Syn.app (Syn.bif "STR") [Syn.var "foo"]
                  , Syn.app (Syn.bif "STR") [Syn.var "_0"]
                  ]
              , Syn.str ""
              ]))
    it "desugars both anaphora with free vars" $
      testDesugar <$>
      parseExpression "\"{foo}{1}{}\"" "test" `shouldBe`
      Right
        (Syn.lam
           ["_0", "_1"]
           (Syn.app
              (Syn.bif "JOIN")
              [ Syn.CoreList
                  [ Syn.app (Syn.bif "STR") [Syn.var "foo"]
                  , Syn.app (Syn.bif "STR") [Syn.var "_1"]
                  , Syn.app (Syn.bif "STR") [Syn.var "_0"]
                  ]
              , Syn.str ""
              ]))
