module Eucalypt.Driver.ImportHandlerSpec
  ( main
  , spec
  ) where

import Data.Maybe (fromJust)
import Eucalypt.Core.AnonSyn
import Eucalypt.Core.Import
import Eucalypt.Core.Syn (CoreExp)
import Eucalypt.Driver.ImportHandler
import Eucalypt.Syntax.Input
import Path
import Test.Hspec

main :: IO ()
main = hspec spec

testIH :: ImportHandler
testIH = importHandler $ fromJust $ parseAbsDir "/"

testRead :: CoreExp v -> Maybe [Input]
testRead = readImports testIH

spec :: Spec
spec =
  describe "Import Handler" $ do
    it "recognises imports in { import: \"a=blah\" }" $
      testRead (block [element "import" $ str "a=blah"] :: CoreExpr) `shouldBe`
      pure <$>
      parseInputFromString "a=blah"
    it "recognises imports in { import: [\"a=blah\"] }" $
      testRead (block [element "import" $ corelist [str "a=blah"]] :: CoreExpr) `shouldBe`
      pure <$>
      parseInputFromString "a=blah"
    it "recognises imports in { import: [\"a=blah\", \"b=foo\"] }" $
      testRead
        (block [element "import" $ corelist [str "a=blah", str "b=foo"]] :: CoreExpr) `shouldBe`
      sequence [parseInputFromString "a=blah", parseInputFromString "b=foo"]
