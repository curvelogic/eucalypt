module Eucalypt.Syntax.InputSpec (spec, main)
  where

import Network.URI
import Test.Hspec
import Data.Maybe ( fromJust, isNothing )
import Eucalypt.Syntax.Input

spec :: Spec
spec = do
  describe "infer format" $ do
    it "recognises json" $
      (inferFormat . URLInput . fromJust . parseRelativeReference) "data.json" `shouldBe`
      Just "json"
    it "recognises toml" $
      (inferFormat . URLInput . fromJust . parseRelativeReference) "data.toml" `shouldBe`
      Just "toml"
  describe "parseInput" $ do
    it "parses simple.eu" $
      parseInputFromString "simple.eu" `shouldBe`
      Just
        Input
          { inputLocator = (URLInput . fromJust . parseURI) "file:simple.eu"
          , inputName = Nothing
          , inputFormat = "eu"
          }
    it "parses simple.json" $
      parseInputFromString "simple.json" `shouldBe`
      Just
        Input
          { inputLocator = (URLInput . fromJust . parseURI) "file:simple.json"
          , inputName = Nothing
          , inputFormat = "json"
          }
    it "parses json@simple.txt" $
      parseInputFromString "json@simple.txt" `shouldBe`
      Just
        Input
          { inputLocator = (URLInput . fromJust . parseURI) "file:simple.txt"
          , inputName = Nothing
          , inputFormat = "json"
          }
    it "parses data=yaml@data.txt" $
      parseInputFromString "data=yaml@data.txt" `shouldBe`
      Just
        Input
          { inputLocator = (URLInput . fromJust . parseURI) "file:data.txt"
          , inputName = Just "data"
          , inputFormat = "yaml"
          }
    it "parses test.yaml" $
      parseInputFromString "test.yaml" `shouldBe`
      Just
        Input
          { inputLocator = (URLInput . fromJust . parseURI) "file:test.yaml"
          , inputName = Nothing
          , inputFormat = "yaml"
          }
    it "fails to parse null" $
      parseInputFromString "\0" `shouldSatisfy` isNothing
    it "parses http(s) URIs" $
      parseInputFromString "https://blah.com/blah.eu" `shouldBe`
      Just
        Input
          { inputLocator =
              (URLInput . fromJust . parseURI) "https://blah.com/blah.eu"
          , inputName = Nothing
          , inputFormat = "eu"
          }
    it "parses http(s) URIs with other specifiers" $
      parseInputFromString "k=yaml@https://blah.com/blah.txt" `shouldBe`
      Just
        Input
          { inputLocator =
              (URLInput . fromJust . parseURI) "https://blah.com/blah.txt"
          , inputName = Just "k"
          , inputFormat = "yaml"
          }
    it "parses resource URIs" $
      parseInputFromString "eu@resource:prelude" `shouldBe`
      Just
        Input
          { inputLocator = ResourceInput "prelude"
          , inputName = Nothing
          , inputFormat = "eu"
          }
    it "parses resource URIs in brackets" $
      parseInputFromString "eu@[resource:prelude]" `shouldBe`
      Just
        Input
          { inputLocator = ResourceInput "prelude"
          , inputName = Nothing
          , inputFormat = "eu"
          }
    it "normalises resource locators" $
      (normaliseLocator . URLInput <$> parseURI "resource:prelude") `shouldBe`
      Just (ResourceInput "prelude")

main :: IO ()
main = hspec spec
