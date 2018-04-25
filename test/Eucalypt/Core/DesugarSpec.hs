module Eucalypt.Core.DesugarSpec (main, spec)
  where

import Eucalypt.Syntax.Ast
import qualified Eucalypt.Core.Syn as Syn
import Eucalypt.Core.Desugar
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "Core" $ do

    it "represents literals" $
      desugarLiteral (VInt 8) `shouldBe` Syn.CoreInt 8

    it "transforms simple declarations" $

      desugarDeclarationForm (prop "x" (op "+" (int 2) (int 5)))

       `shouldBe`

      ("x", Syn.CoreApp (Syn.CoreApp (Syn.CoreVar "+") (Syn.int 2)) (Syn.int 5))

    it "takes leading underscores to indicate built-ins" $

      desugar (EIdentifier [NormalName "__NULL"]) `shouldBe` Syn.bif "NULL"

    it "translates lookups against builtins" $

      desugar (EIdentifier [NormalName "__X", NormalName "id"]) `shouldBe` Syn.CoreLookup (Syn.bif "X") "id"
