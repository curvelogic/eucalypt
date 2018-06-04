{-# LANGUAGE LambdaCase #-}

module Eucalypt.Core.DesugarSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Desugar
import qualified Eucalypt.Core.Syn as Syn
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

coreSpec :: Spec
coreSpec =
  describe "Core" $ do
    it "represents literals" $ desugarLiteral (VInt 8) `shouldBe` Syn.CoreInt 8
    it "transforms simple declarations" $
      desugarDeclarationForm
        Annotated
          { annotation = Nothing
          , declaration = prop "x" (opsoup [int 2, operatorName "+", int 5])
          } `shouldBe`
      (Nothing, "x", Syn.soup [Syn.int 2, Syn.var "+", Syn.int 5])
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
        (desugarBlock (at nowhere $ Block [ann (str "docs") (prop "x" (int 5))])) `shouldSatisfy` \case
        Syn.CoreBlock (Syn.CoreList [Syn.CoreMeta _ _]) -> True
        _ -> False

soupSpec :: Spec
soupSpec =
  describe "soup desugaring" $ do
    it "inserts call operators" $
      desugarSoup
        [int 5, normalName "x", normalName "f", applyTuple [int 4, int 7]] `shouldBe`
      Syn.soup
        [ Syn.int 5
        , Syn.var "x"
        , Syn.var "f"
        , Syn.callOp
        , Syn.args [Syn.int 4, Syn.int 7]
        ]
    it "handles iterated calls" $
      desugarSoup
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
      desugarSoup
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
      desugarBlock (at nowhere $ Block [bare (prop "x" (normalName "y"))]) `shouldBe`
      Syn.letexp
        [("x", Syn.var "y")]
        (Syn.block [Syn.element "x" $ Syn.var "x"])
    it "handles built-ins" $
      desugarBlock
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
      desugar <$>
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
      desugar <$>
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
      desugar <$>
      parseExpression "x - 1" "test" `shouldBe`
      Right (Syn.soup [Syn.var "x", Syn.var "-", Syn.int 1])
