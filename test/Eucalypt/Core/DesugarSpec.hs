{-# LANGUAGE LambdaCase #-}
module Eucalypt.Core.DesugarSpec (main, spec)
  where

import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import qualified Eucalypt.Core.Syn as Syn
import Eucalypt.Core.Desugar
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  coreSpec
  soupSpec

coreSpec :: Spec
coreSpec =

  describe "Core" $ do

    it "represents literals" $
      desugarLiteral (VInt 8) `shouldBe` Syn.CoreInt 8

    it "transforms simple declarations" $

      desugarDeclarationForm Annotated{annotation=Nothing,declaration=prop "x" (op "+" (int 2) (int 5))}

       `shouldBe`

      (Nothing, "x", Syn.CoreApply (Syn.CoreVar "+") [Syn.int 2, Syn.int 5])

    it "takes leading underscores to indicate built-ins" $

      desugar (at nowhere (EIdentifier [NormalName "__NULL"])) `shouldBe` Syn.bif "NULL"

    it "translates lookups against builtins" $

      desugar (at nowhere (EIdentifier [NormalName "__X", NormalName "id"])) `shouldBe` Syn.CoreLookup (Syn.bif "X") "id"

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

      Syn.unbind (desugarBlock (at nowhere $ Block [ann (str "docs") (prop "x" (int 5))]))
      `shouldSatisfy`
      \case
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
         , callOp
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
         , callOp
         , Syn.args [Syn.corename "x"]
         , callOp
         , Syn.args [Syn.corename "y"]
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
         [Syn.var "x", lookupOp, Syn.corename "y", lookupOp, Syn.corename "z"]
