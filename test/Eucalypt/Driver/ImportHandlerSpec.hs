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

testRead :: CoreExp v -> [Input]
testRead = map fst . readImports testIH

testRepo :: String
testRepo = "https://github.com/gmorpheme/eu.aws.git"

testCommit :: String
testCommit = "b230b6d4e2a9b1242806bb89c2973547a0eaf45e"

testFile :: String
testFile = "cloudformation.eu"

spec :: Spec
spec =
  describe "Import Handler" $ do
    context "Simple imports" $ do
      it "recognises imports in { import: \"a=blah\" }" $
        testRead (block [element "import" $ str "a=blah"] :: CoreExpr) `shouldBe`
        [fromJust $ parseInputFromString "a=blah"]
      it "recognises imports in { import: [\"a=blah\"] }" $
        testRead
          (block [element "import" $ corelist [str "a=blah"]] :: CoreExpr) `shouldBe`
        [fromJust $ parseInputFromString "a=blah"]
      it "recognises imports in { import: [\"a=blah\", \"b=foo\"] }" $
        testRead
          (block [element "import" $ corelist [str "a=blah", str "b=foo"]] :: CoreExpr) `shouldBe`
        map (fromJust . parseInputFromString) ["a=blah", "b=foo"]
    context "Git imports" $ do
      it "recognises imports in single git import" $
        testRead
          (block
             [ element "import" $
               letblock
                 [ ("r", str testRepo)
                 , ("c", str testCommit)
                 , ("i", str testFile)
                 ] $
               block
                 [ element "git" $ var "r"
                 , element "commit" $ var "c"
                 , element "import" $ var "i"
                 ]
             ]) `shouldBe`
        [ fromJust $
          parseInputFromString
            ("/.cache/" ++ testCommit ++ "/cloudformation.eu")
        ]
      it "recognises imports in double git import" $
        testRead
          (block
             [ element "import" $
               letblock
                 [ ("r", str testRepo)
                 , ("c", str testCommit)
                 , ("i", corelist [str "a=foo", str "b=bar"])
                 ] $
               block
                 [ element "git" $ var "r"
                 , element "commit" $ var "c"
                 , element "import" $ var "i"
                 ]
             ]) `shouldBe`
        map
          (fromJust . parseInputFromString)
          [ "a=/.cache/" ++ testCommit ++ "/foo"
          , "b=/.cache/" ++ testCommit ++ "/bar"
          ]
      it "recognises imports in two git imports" $
        testRead
          (block
             [ element "import" $
               corelist
                 [ letblock
                     [ ("r", str testRepo)
                     , ("c", str testCommit)
                     , ("i", corelist [str "a=foo"])
                     ] $
                   block
                     [ element "git" $ var "r"
                     , element "commit" $ var "c"
                     , element "import" $ var "i"
                     ]
                 , letblock
                     [ ("r", str testRepo)
                     , ("c", str testCommit)
                     , ("i", corelist [str "b=bar"])
                     ] $
                   block
                     [ element "git" $ var "r"
                     , element "commit" $ var "c"
                     , element "import" $ var "i"
                     ]
                 ]
             ]) `shouldBe`
        map
          (fromJust . parseInputFromString)
          [ "a=/.cache/" ++ testCommit ++ "/foo"
          , "b=/.cache/" ++ testCommit ++ "/bar"
          ]
