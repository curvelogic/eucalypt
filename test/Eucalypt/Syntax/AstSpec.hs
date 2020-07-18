module Eucalypt.Syntax.AstSpec
  ( main
  , spec
  ) where

import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "int literals" $
    it "represents integers" $ do
      locatee (int 5) `shouldBe` (ELiteral . VInt) 5
      locatee (int (-5)) `shouldBe` (ELiteral . VInt) (-5)
  describe "string literals" $
    it "represents strings" $
    locatee (str "1234") `shouldBe` (ELiteral . VStr) "1234"
  describe "list literals" $
    it "represents lists" $
    locatee (list (map normalName ["x", "y", "z"])) `shouldBe`
    EList (map normalName ["x", "y", "z"])
  describe "property declaration" $
    it "represents a property declaration" $
    locatee (prop "x" (normalName "expr")) `shouldBe`
    PropertyDecl (NormalName "x") (normalName "expr")
  describe "function declaration" $
    it "represents function declarations" $
    locatee (func "x" ["a", "b", "c"] (normalName "body")) `shouldBe`
    FunctionDecl (NormalName "x") ["a", "b", "c"] (normalName "body")
  describe "oper smart constructor" $
    it "represents operator declarations" $
    locatee (oper "<<&&>>" "lhs" "rhs" (normalName "body")) `shouldBe`
    OperatorDecl (OperatorName "<<&&>>") "lhs" "rhs" (normalName "body")
