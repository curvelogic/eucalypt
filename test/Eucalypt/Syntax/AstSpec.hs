module Eucalypt.Syntax.AstSpec (main, spec)
where

import Eucalypt.Syntax.Ast
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "ident smart constructor" $ do

    it "creates qualified names" $ do
      ident "x.y.z" `shouldBe` EIdentifier [NormalName "x", NormalName "y", NormalName "z"]

    it "creates simple names" $ do
      ident "x" `shouldBe` EIdentifier [NormalName "x"]

  describe "op smart constructor" $ do

    it "creates operator calls" $ do
      op "<*>" (ident "x") (ident "y") `shouldBe` EOperation (OperatorName "<*>") (ident "x") (ident "y")

  describe "cat smart constructor" $ do

    it "creates catenations" $ do
      cat (ident "x") (ident "y") `shouldBe` ECatenation (ident "x") (ident "y")

  describe "invoke smart constructor" $ do

    it "creates invocations" $ do
      invoke (ident "x.y.fn") (map ident ["a", "b", "c"]) `shouldBe` EInvocation (ident "x.y.fn") [ident "a", ident "b", ident "c"]

  describe "int literals" $ do
    it "represents integers" $ do
      (int 5) `shouldBe` ((ELiteral . VInt) 5)
      (int (-5)) `shouldBe` ((ELiteral . VInt) (-5))

  describe "string literals" $ do
    it "represents strings" $ do
      (str "1234") `shouldBe` (ELiteral . VStr) "1234"

  describe "list literals" $ do
    it "represents lists" $ do
      (list $ map ident ["x", "y", "z"]) `shouldBe` (EList $ map ident ["x", "y", "z"])

  describe "property declaration" $ do
    it "represents a property declaration" $ do
      (prop "x" (ident "some.expr")) `shouldBe` (PropertyDecl
                                                 (NormalName "x")
                                                 (ident "some.expr"))

  describe "function declaration" $ do
    it "represents function declarations" $ do
      (func "x" ["a", "b", "c"] (ident "body")) `shouldBe` (FunctionDecl
                                                            (NormalName "x")
                                                            (map NormalName ["a", "b", "c"])
                                                            (ident "body"))

  describe "oper smart constructor" $ do
    it "represents operator declarations" $ do
      (oper "<<&&>>" "lhs" "rhs" (ident "body")) `shouldBe` (OperatorDecl
                                                             (OperatorName "<<&&>>")
                                                             (NormalName "lhs")
                                                             (NormalName "rhs")
                                                             (ident "body"))
