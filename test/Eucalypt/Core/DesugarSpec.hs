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
      desugarLiteral (VInt 8) `shouldBe` Int 8

    it "transforms simple declarations" $

      desugarDeclarationForm (prop "x" (op "+" (int 2) (int 5)))

       `shouldBe`

      (fromStr "x", App (App (Var $ fromStr "+") (Prim (Int 2))) (Prim (Int 5)))
