{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Eucalypt.Syntax.ParseExprSpec
  ( main
  , spec
  ) where

import Data.Void
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.ParseExpr
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec

delocated :: Parser Expression -> Parser Expression
delocated = fmap stripLocation

testParse :: Parser Expression -> String -> Either (ParseError Char Void) Expression
testParse p = parse (delocated p <* eof) ""



main :: IO ()
main = hspec spec

-- ? name generators
--
--

lower = ['a' .. 'z']

upper = ['A' .. 'Z']

digits = ['0' .. '9']

idStartPunc = "$?_"

idContPunc = "$?!_-*"

operatorStart = "!@£%^&*|></+=-~"

operatorCont = "!@£$%^&*|></?+=-~"

identifierStart = lower ++ upper ++ digits ++ idStartPunc

identifierCont = lower ++ upper ++ digits ++ idContPunc

validNormalNames :: Gen String
validNormalNames =
  (:) <$> oneof (map return identifierStart) <*>
  listOf1 (oneof (map return identifierCont))

validOperatorNames :: Gen String
validOperatorNames = gen `suchThat` (/= "//")
  where
    gen =
      (:) <$> oneof (map return operatorStart) <*>
      listOf (oneof (map return operatorCont))

validSQNames :: Gen String
validSQNames = fmap (\x -> "'" ++ filter (/= '\'') x ++ "'") arbitrary

spec :: Spec
spec = do
  identifierSpec
  primitiveSpec
  expressionSpec

identifierSpec :: Spec
identifierSpec = do
  describe "parsing normal identifiers" $ do
    it "accepts normal identifiers" $
      parse normalIdentifier "" "abc" `shouldParse` "abc"
    it "rejects {{{" $
      parse normalIdentifier "" `shouldFailOn` "{{{"
    it "rejects 'x'" $
      parseMaybe normalIdentifier "'x'" `shouldBe` Nothing
    it "accepts legal identifiers" $
      forAll validNormalNames (\s -> parseMaybe normalIdentifier s == Just s)
  describe "parsing operator identifiers" $ do
    it "accepts operator identifiers" $
      parseMaybe operatorIdentifier "++<" `shouldBe` Just "++<"
    it "accepts legal operator names" $
      forAll validOperatorNames (\s -> parseMaybe operatorIdentifier s == Just s)
  describe "parsing single quoted identifiers" $ do
    it "accepts single quoted identifiers" $
      parseMaybe quotedIdentifier "'(*&^(*&jhgijhg&*&~~~))'" `shouldBe` Just "(*&^(*&jhgijhg&*&~~~))"
    it "accepts legal single quoted names" $
      forAll validSQNames (\s -> parseMaybe quotedIdentifier s == Just (unquote s))


parsesAnyInteger :: Integer -> Bool
parsesAnyInteger i = parseMaybe integer (show i) == (Just . VInt) i

parsesDoubles :: Double -> Bool
parsesDoubles d = parseMaybe float (show d) == (Just . VFloat) d

parsesSymbols :: String -> Bool
parsesSymbols s = parseMaybe symbolLiteral (':':s) == (Just . VSym) s

parsesQuotedSymbols :: String -> Bool
parsesQuotedSymbols s = parseMaybe symbolLiteral (':':s) == (Just . VSym . unquote) s

primitiveSpec :: Spec
primitiveSpec = do
  describe "parsing numbers" $ do
    it "parses shown haskell integers" $ property parsesAnyInteger
    it "parses shown haskell float" $ property parsesDoubles
  describe "parsing strings" $
    it "parses simple strings" $
    parseMaybe stringLiteral "\"abc\"" == Just (VStr "abc")
  describe "parsing symbols" $ do
    it "parses normal symbols" $ forAll validNormalNames parsesSymbols
    it "parses operator symbols" $ forAll validOperatorNames parsesSymbols
    it "parses quoted symbols" $ forAll validSQNames parsesQuotedSymbols
  describe "parsing primitives" $ do
    it "parses numbers to numbers" $ do
      testParse primitive "1234" `shouldParse` at nowhere (ELiteral (VInt 1234))
      testParse primitive "1234.1234" `shouldParse`
        at nowhere (ELiteral (VFloat 1234.1234))
      testParse primitive "-1234" `shouldParse`
        at nowhere (ELiteral (VInt (-1234)))
      testParse primitive "-1234.1234" `shouldParse`
        at nowhere (ELiteral (VFloat (-1234.1234)))
    it "parses symbols to symbols" $ do
      testParse primitive ":blah" `shouldParse`
        at nowhere (ELiteral (VSym "blah"))
      testParse primitive ":'blah'" `shouldParse`
        at nowhere (ELiteral (VSym "blah"))
    it "parses strings to strings" $
      testParse primitive "\"blah\"" `shouldParse`
      at nowhere (ELiteral (VStr "blah"))


expressionSpec :: Spec
expressionSpec =
  describe "expression parsing" $ do
    it "accepts primitives" $
      testParse expression "1234" `shouldParse`
      at nowhere (ELiteral (VInt 1234))
    it "accepts parenthesised sequences" $
      testParse expression "(33 f(1,2,3) //=> 5)" `shouldParse`
      opsoupParens
        [ int 33
        , invoke (normalName "f") [int 1, int 2, int 3]
        , operatorName "//=>"
        , int 5
        ]
    it "accepts non-paren sequences" $
      testParse expression "1 3 ** f(a)(b)" `shouldParse`
      opsoup
        [ int 1
        , int 3
        , operatorName "**"
        , invoke (invoke (normalName "f") [normalName "a"]) [normalName "b"]
        ]
    it "accepts iterated calls" $
      testParse expression "f(a)(b)(c)" `shouldParse`
      invoke
        (invoke (invoke (normalName "f") [normalName "a"]) [normalName "b"])
        [normalName "c"]
    it "accepts calls on paren exprs" $
      testParse expression "(x op)(b)(c)" `shouldParse`
      invoke
        (invoke
           (opsoupParens [normalName "x", normalName "op"])
           [normalName "b"])
        [normalName "c"]
    it "rejects lonely colons" $ do
      testParse expression `shouldFailOn` "a : a"
      testParse expression `shouldFailOn` "a : a"
      testParse expression `shouldFailOn` "a : a"
      testParse expression `shouldFailOn` "a:"
      testParse expression `shouldFailOn` "a:a"
      testParse expression `shouldSucceedOn` ":a"
    it "rejects commas outside arg lists" $ do
      testParse expression `shouldFailOn` "a , a"
      testParse expression `shouldFailOn` ", a"
      testParse expression `shouldFailOn` "a ,"
      testParse expression `shouldFailOn` "a,a"
      testParse expression `shouldFailOn` ",a"
      testParse expression `shouldFailOn` "a,"
