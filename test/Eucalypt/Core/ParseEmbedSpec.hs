module Eucalypt.Core.ParseEmbedSpec
  ( main
  , spec
  ) where

import Control.Monad.State.Strict
import Eucalypt.Core.ParseEmbed
import Eucalypt.Core.Desugar
import Eucalypt.Core.Import
import qualified Eucalypt.Core.Syn as Syn
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.ParseExpr
import Test.Hspec

testParseEmbed :: Expression -> Syn.CoreExpr
testParseEmbed =
  (`evalState` initTranslateState 1 nullImportHandler) . unTranslate . translateEmbedded

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "samples" $ do
  it "parses c-var" $
    testParseEmbed <$> parseExpression "[:c-var, \"x\"]" "test" `shouldBe`
    Right (Syn.var 1 "x")
