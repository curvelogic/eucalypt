module Eucalypt.Syntax.ParseStringSpec
  ( main
  , spec
  ) where

import Data.Void
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.ParseString
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = hspec spec

anaphor :: StringChunk
anaphor =
  Interpolation $
  InterpolationRequest
    { refTarget = Anaphor Nothing
    , refParseOrFormat = Nothing
    , refConversion = Nothing
    }

anaphor1 :: StringChunk
anaphor1 =
  Interpolation $
  InterpolationRequest
    { refTarget = Anaphor (Just 1)
    , refParseOrFormat = Nothing
    , refConversion = Nothing
    }

refer :: String -> StringChunk
refer v =
  Interpolation $
  InterpolationRequest
    { refTarget = Reference v
    , refParseOrFormat = Nothing
    , refConversion = Nothing
    }

testParse :: String -> Either (ParseErrorBundle String Void) [StringChunk]
testParse = parse quotedStringContent "<<test>>"


spec :: Spec
spec =
  describe "string parsing" $ do
    it "reads simple content - \"foo\"" $
      testParse "foo" `shouldParse`
      [LiteralContent "foo"]
    it "handles escaped braces - \"{{foo}}\"" $
      testParse "{{foo}}" `shouldParse`
      [LiteralContent "{foo}"]
    it "handles blank anaphor - \"{}\"" $
      testParse "{}" `shouldParse` [anaphor]
    it "handles numeric anaphor - \"{1}\"" $
      testParse "{1}" `shouldParse` [anaphor1]
    it "handles interpolation ref - \"{foo}\"" $
      testParse "{foo}" `shouldParse` [refer "foo"]
    it "handles mixture - \"x{}y{}z{}a{foo}b{bar}c{baz}{{txt}}\"" $
      testParse "x{}y{}z{}a{foo}b{bar}c{baz}{{txt}}" `shouldParse`
      [ LiteralContent "x"
      , anaphor
      , LiteralContent "y"
      , anaphor
      , LiteralContent "z"
      , anaphor
      , LiteralContent "a"
      , refer "foo"
      , LiteralContent "b"
      , refer "bar"
      , LiteralContent "c"
      , refer "baz"
      , LiteralContent "{txt}"
      ]
