module Eucalypt.Core.DesugarSpec (main, spec)
  where

import Eucalypt.Syntax.Ast
import Eucalypt.Core.Syn
import Eucalypt.Core.Desugar
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "Core" $ do

    it "represents literals" $
      desugarLiteral (VInt 8) `shouldBe` CoreInt 8

    it "transforms simple declarations" $

      desugarDeclarationForm (prop "x" (op "+" (int 2) (int 5)))

       `shouldBe`

      ("x", CoreApp (CoreApp (CoreVar "+") (CorePrim (CoreInt 2))) (CorePrim (CoreInt 5)))
