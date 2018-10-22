module Eucalypt.Core.MetadataSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Metadata
import Eucalypt.Core.Syn (CoreExpr)
import Eucalypt.Core.AnonSyn
import Eucalypt.Syntax.Input
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "import recognition" $
  it "recognises imports in { import: \"a=blah\" }" $
  importsFromMetadata (block [element "import" $ str "a=blah"] :: CoreExpr) `shouldBe`
  pure <$>
  parseInputFromString "a=blah"
