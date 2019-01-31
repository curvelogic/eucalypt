{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Eucalypt.Syntax.ParseExprSpec
  ( main
  , spec
  ) where

import Data.Void
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.ParseCommon
import Eucalypt.Syntax.ParseExpr
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec

delocated :: HasLocation a => Parser a -> Parser a
delocated = fmap stripLocation

testParse :: HasLocation a => Parser a -> String -> Either (ParseErrorBundle String Void) a
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

identifierStart = lower ++ upper ++ idStartPunc

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
  blockSpec

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
    it "accepts \"'~'\"" $
      parseMaybe quotedIdentifier "'~'" `shouldBe` Just "~"
    it "accepts single quoted identifiers" $
      parseMaybe quotedIdentifier "'(*&^(*&jhgijhg&*&~~~))'" `shouldBe` Just "(*&^(*&jhgijhg&*&~~~))"
    it "accepts legal single quoted names" $
      forAll validSQNames (\s -> parseMaybe quotedIdentifier s == Just (unquote s))
  describe "parsing names" $ do
    it "rejects qualified names (these are now operators)" $
      testParse name `shouldFailOn` "x.y.z"
    it "rejects qualified quoted names (these are now operators)" $
      testParse name `shouldFailOn` "'x'.'y'.'z'"

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
  describe "parsing strings" $ do
    it "parses simple strings" $
      testParse stringLiteral "\"abc\"" `shouldParse` str "abc"
    it "parses empty string" $
      testParse stringLiteral "\"\"" `shouldParse` str ""
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
    it "accepts primitives" $ testParse expression "1234" `shouldParse` int 1234
    it "accepts parenthesised sequences" $
      testParse expression "(33 f(1,2,3) //=> 5)" `shouldParse`
      opsoupParens
        [ int 33
        , normalName "f"
        , applyTuple [int 1, int 2, int 3]
        , operatorName "//=>"
        , int 5
        ]
    it "accepts non-paren sequences" $
      testParse expression "1 3 ** f(a)(b)" `shouldParse`
      opsoup
        [ int 1
        , int 3
        , operatorName "**"
        , normalName "f"
        , applyTuple [normalName "a"]
        , applyTuple [normalName "b"]
        ]
    it "accepts iterated calls" $
      testParse expression "f(a)(b)(c)" `shouldParse`
      opsoup
        [ normalName "f"
        , applyTuple [normalName "a"]
        , applyTuple [normalName "b"]
        , applyTuple [normalName "c"]
        ]
    it "accepts calls on paren exprs" $
      testParse expression "(x op)(b)(c)" `shouldParse`
      opsoup
        [ opsoupParens [normalName "x", normalName "op"]
        , applyTuple [normalName "b"]
        , applyTuple [normalName "c"]
        ]
    it "does not confuse gen lookup with call syntax" $
      testParse expression "{ a: x }.(a + a)" `shouldParse`
      opsoup
        [ block [bare $ prop "a" $ normalName "x"]
        , operatorName "."
        , opsoupParens [normalName "a", operatorName "+", normalName "a"]
        ]
    it "rejects lonely colons" $ do
      testParse expression `shouldFailOn` "a : a"
      testParse expression `shouldFailOn` "a:"
      testParse expression `shouldSucceedOn` ":a"
    it "rejects commas outside arg lists" $ do
      testParse expression `shouldFailOn` "a , a"
      testParse expression `shouldFailOn` ", a"
      testParse expression `shouldFailOn` "a ,"
      testParse expression `shouldFailOn` "a,a"
      testParse expression `shouldFailOn` ",a"
      testParse expression `shouldFailOn` "a,"
    it "accepts []" $ testParse expression "[]" `shouldParse` list []
    it "accepts list literals" $ do
      testParse expression "[1, 2, 3] [a, b, c] f([a,b])" `shouldParse`
        opsoup
          [ list [int 1, int 2, int 3]
          , list [normalName "a", normalName "b", normalName "c"]
          , normalName "f"
          , applyTuple [list [normalName "a", normalName "b"]]
          ]
      testParse expression "[l t, l h]" `shouldParse`
        list
          [ opsoup [normalName "l", normalName "t"]
          , opsoup [normalName "l", normalName "h"]
          ]
    it "accepts qualified names" $ do
      testParse expression "x.y.z" `shouldParse`
        opsoup
          [ normalName "x"
          , operatorName "."
          , normalName "y"
          , operatorName "."
          , normalName "z"
          ]
      testParse expression "'x'.'y'.'z'" `shouldParse`
        opsoup
          [ normalName "x"
          , operatorName "."
          , normalName "y"
          , operatorName "."
          , normalName "z"
          ]
    it "accepts 'foo + bar'" $
      testParse expression "foo + bar" `shouldParse`
      opsoup [normalName "foo", operatorName "+", normalName "bar"]
    it "accepts 'foo & bar & baz'" $
      testParse expression "foo & bar & baz" `shouldParse`
      opsoup
        [ normalName "foo"
        , operatorName "&"
        , normalName "bar"
        , operatorName "&"
        , normalName "baz"
        ]
    it "accepts x.y.z *^&%^ x.y.y" $
      testParse expression "x.y.z *^&%^ x.y.y" `shouldParse`
      opsoup
        [ normalName "x"
        , operatorName "."
        , normalName "y"
        , operatorName "."
        , normalName "z"
        , operatorName "*^&%^"
        , normalName "x"
        , operatorName "."
        , normalName "y"
        , operatorName "."
        , normalName "y"
        ]
    it "accepts x y" $
      testParse expression "x y" `shouldParse`
      opsoup [normalName "x", normalName "y"]
    it "parses f(x, y)" $
      testParse expression "f(x, y)" `shouldParse`
      opsoup [normalName "f", applyTuple [normalName "x", normalName "y"]]
    it "parses f(zz)" $
      testParse expression "f(zz)" `shouldParse`
      opsoup [normalName "f", applyTuple [normalName "zz"]]
    it "parses foo.bar.baz(quux)" $
      testParse expression "foo.bar.baz(quux)" `shouldParse`
      opsoup
        [ normalName "foo"
        , operatorName "."
        , normalName "bar"
        , operatorName "."
        , normalName "baz"
        , applyTuple [normalName "quux"]
        ]
    it "parses foo.bar.'baz'(quux)" $
      testParse expression "foo.bar.'baz'(quux)" `shouldParse`
      opsoup
        [ normalName "foo"
        , operatorName "."
        , normalName "bar"
        , operatorName "."
        , normalName "baz"
        , applyTuple [normalName "quux"]
        ]
    it "parses if(foo(bar),baz,quux) " $
      testParse expression "if(foo(bar),baz,quux)" `shouldParse`
      opsoup
        [ normalName "if"
        , applyTuple
            [ opsoup [normalName "foo", applyTuple [normalName "bar"]]
            , normalName "baz"
            , normalName "quux"
            ]
        ]
    it "parses if(car(xs), concat(reverse(xs),[car(xs)]), nil)" $
      testParse expression "if(car(xs), concat(reverse(xs),[car(xs)]), nil)" `shouldParse`
      opsoup
        [ normalName "if"
        , applyTuple
            [ opsoup [normalName "car", applyTuple [normalName "xs"]]
            , opsoup
                [ normalName "concat"
                , applyTuple
                    [ opsoup
                        [normalName "reverse", applyTuple [normalName "xs"]]
                    , list
                        [ opsoup
                            [normalName "car", applyTuple [normalName "xs"]]
                        ]
                    ]
                ]
            , normalName "nil"
            ]
        ]
    it "parses x y |<| z" $
      testParse expression "x y |<| z" `shouldParse`
      opsoup
        [normalName "x", normalName "y", operatorName "|<|", normalName "z"]
    it "parses g. h .'i'" $
      testParse expression "g. h .'i'" `shouldParse`
      opsoup
        [ normalName "g"
        , operatorName "."
        , normalName "h"
        , operatorName "."
        , normalName "i"
        ]
    it "parses 987" $ testParse expression "987" `shouldParse` int 987
    it "parses x y z" $
      testParse expression "x y z" `shouldParse`
      opsoup [normalName "x", normalName "y", normalName "z"]
    it "parses y (x f)" $
      testParse expression "y (x f)" `shouldParse`
      opsoup [normalName "y", opsoupParens [normalName "x", normalName "f"]]
    it "parses y f(x)" $
      testParse expression "y f(x)" `shouldParse`
      opsoup [normalName "y", normalName "f", applyTuple [normalName "x"]]
    it "parses y (f(x))" $
      testParse expression "y (f(x))" `shouldParse`
      opsoup
        [ normalName "y"
        , opsoupParens [normalName "f", applyTuple [normalName "x"]]
        ]
    it "fails x :" $ testParse expression `shouldFailOn` "x :"
    it "fails h(y) :" $ testParse expression `shouldFailOn` "h(y) :"
    it "fails (gg++yy) :" $ testParse expression `shouldFailOn` "(gg++yy) :"
    it "parses xs f(_.z = 1) correctly" $
      testParse expression "xs f(_.z = 1)" `shouldParse`
      opsoup
        [ normalName "xs"
        , normalName "f"
        , applyTuple
            [ opsoup
                [ normalName "_"
                , operatorName "."
                , normalName "z"
                , operatorName "="
                , int 1
                ]
            ]
        ]
    it "parses xs f(_.z = :k) correctly" $
      testParse expression "xs f(_.z = :k)" `shouldParse`
      opsoup
        [ normalName "xs"
        , normalName "f"
        , applyTuple
            [ opsoup
                [ normalName "_"
                , operatorName "."
                , normalName "z"
                , operatorName "="
                , sym "k"
                ]
            ]
        ]
    it "parses ⊡ 2 ⨈ correctly" $
      testParse expression "⊡ :k ⨈" `shouldParse`
      opsoup [operatorName "⊡", sym "k", operatorName "⨈"]


blockSpec :: Spec
blockSpec = do
  describe "parsing property declarations" $ do
    it "accepts 'a: b'" $
      testParse propertyDeclaration "a: b" `shouldParse`
      prop "a" (normalName "b")
    it "accepts 'a : b'" $
      testParse propertyDeclaration "a : b" `shouldParse`
      prop "a" (normalName "b")
  describe "parsing function declarations" $ do
    it "accepts 'f(x): y'" $
      testParse functionDeclaration "f(x): y" `shouldParse`
      func "f" ["x"] (normalName "y")
    it "accepts 'f(x) : y'" $
      testParse functionDeclaration "f(x) : y" `shouldParse`
      func "f" ["x"] (normalName "y")
    it "accepts 'f(x, y): z'" $
      testParse functionDeclaration "f(x, y): z" `shouldParse`
      func "f" ["x", "y"] (normalName "z")
    it "accepts 'f(x,y): z z z'" $
      testParse functionDeclaration "f(x,y): z z z" `shouldParse`
      func
        "f"
        ["x", "y"]
        (opsoup [normalName "z", normalName "z", normalName "z"])
  describe "parsing operator declarations" $ do
    it "accepts '(a + b): z(a, b)'" $
      testParse operatorDeclaration "(a + b): z(a, b)" `shouldParse`
      oper
        "+"
        "a"
        "b"
        (opsoup [normalName "z", applyTuple [normalName "a", normalName "b"]])
    it "accepts '(a+b): z(a,b) '" $
      testParse operatorDeclaration "(a+b): z(a,b)" `shouldParse`
      oper
        "+"
        "a"
        "b"
        (opsoup [normalName "z", applyTuple [normalName "a", normalName "b"]])
    it "parses (foo *^^* bar) : bar foo(344)" $
      testParse operatorDeclaration "(foo *^^* bar) : bar foo(344)" `shouldParse`
      oper
        "*^^*"
        "foo"
        "bar"
        (opsoup [normalName "bar", normalName "foo", applyTuple [int 344]])
    it "parses (££ x): 6 / x" $
      testParse prefixOperatorDeclaration "(££ x): 6 / x" `shouldParse`
      loper "££" "x" (opsoup [int 6, operatorName "/", normalName "x"])
    it "parses (x ££): 6 / x" $
      testParse postfixOperatorDeclaration "(x ££): 6 / x" `shouldParse`
      roper "££" "x" (opsoup [int 6, operatorName "/", normalName "x"])
  describe "parsing declarations" $ do
    it "accepts '` :k a : b'" $
      testParse anyDeclaration "` :k a : b" `shouldParse`
      ann (sym "k") (prop "a" (normalName "b"))
    it "accepts '` \"s\" a : b'" $
      testParse anyDeclaration "` \"s\" a : b" `shouldParse`
      ann (str "s") (prop "a" (normalName "b"))
    it "accepts '` {x: y} a : b'" $
      testParse anyDeclaration "` {x: y} a : b" `shouldParse`
      ann (block [bare $ prop "x" $ normalName "y"]) (prop "a" (normalName "b"))
    it "accepts 'a: b  '" $
      testParse anyDeclaration "a: b  " `shouldParse`
      bare (prop "a" (normalName "b"))
    it "parses ` a x : y " $
      testParse anyDeclaration "` a x : y " `shouldParse`
      ann (normalName "a") (prop "x" (normalName "y"))
    it "parses ` a b c zz(yy,gg) : y " $
      testParse anyDeclaration "` a b c zz(yy,gg) : y " `shouldParse`
      ann
        (opsoup [normalName "a", normalName "b", normalName "c"])
        (func "zz" ["yy", "gg"] (normalName "y"))
    it "parses ` a (foo + bar) : baz " $
      testParse anyDeclaration "` a (foo + bar) : baz" `shouldParse`
      ann (normalName "a") (oper "+" "foo" "bar" (normalName "baz"))
    it "parses ` { if: :listfn } lists: { } " $
      testParse anyDeclaration "` { if: :listfn } lists: { }" `shouldParse`
      ann (block [bare (prop "if" (sym "listfn"))]) (prop "lists" (block []))
    it "parses items: [\"one\", \"two\", \"three\"]" $
      testParse anyDeclaration "items: [\"one\", \"two\", \"three\"]" `shouldParse`
      bare (prop "items" (list [str "one", str "two", str "three"]))
  describe "blockParsing" $ do
    it "accepts '{a: x}'" $
      testParse blockLiteral "{a: x}" `shouldParse`
      block [bare $ prop "a" $ normalName "x"]
    it "accepts '{a: x }'" $
      testParse blockLiteral "{a: x }" `shouldParse`
      block [bare $ prop "a" $ normalName "x"]
    it "accepts '{ a: 1 b: 2 }'" $
      testParse blockLiteral "{ a: 1 b: 2 }" `shouldParse`
      block [bare $ prop "a" (int 1), bare $ prop "b" (int 2)]
    it "accepts empty blocks" $
      testParse blockLiteral "{}" `shouldParse` block []
    it "accepts metadata" $
      testParse blockLiteral "{` :m a: d}" `shouldParse`
      block [ann (sym "m") (prop "a" (normalName "d"))]
    it "parses { `a x : y }" $
      testParse blockLiteral "{ `a x : y }" `shouldParse`
      block [ann (normalName "a") (prop "x" (normalName "y"))]
    it "parses { `{ z:g } x : y }" $
      testParse blockLiteral "{ `{ z:g } x : y }" `shouldParse`
      block
        [ ann
            (block [bare $ prop "z" $ normalName "g"])
            (prop "x" (normalName "y"))
        ]
    it "parses nested metadata" $
      testParse blockLiteral "{ `{`{c:d} a: b} x : y }" `shouldParse`
      block
        [ ann
            (block
               [ ann
                   (block [bare $ prop "c" (normalName "d")])
                   (prop "a" (normalName "b"))
               ])
            (prop "x" $ normalName "y")
        ]
  describe "unit parsing" $
    it "parses foo : bar baz : quux " $
    testParse unit "foo : bar baz : quux" `shouldParse`
    bareUnit
      [ bare $ prop "foo" $ normalName "bar"
      , bare $ prop "baz" $ normalName "quux"
      ]
  describe "unicode support" $ do
    it "parses unicode operators" $
      testParse unit " (f ∘ g): compose(f, g) " `shouldParse`
      bareUnit
        [ bare
            (oper
               "∘"
               "f"
               "g"
               (opsoup
                  [ normalName "compose"
                  , applyTuple [normalName "f", normalName "g"]
                  ]))
        ]
    it "accepts unicode names" $
      testParse unit " β(ॵ): כֿ(ॵ) " `shouldParse`
      bareUnit
        [ bare
            (func
               "β"
               ["ॵ"]
               (opsoup [normalName "כֿ", applyTuple [normalName "ॵ"]]))
        ]
    it "accepts annotated units" $
      testParse unit "{ import: \"import.eu\" } result: foo" `shouldParse`
      annUnit
        (block [bare $ prop "import" $ str "import.eu"])
        [bare $ prop "result" $ normalName "foo"]
    it "accepts annotated units after comments / ws" $
      testParse unit " { import: \"import.eu\" } result: foo " `shouldParse`
      annUnit
        (block [bare $ prop "import" $ str "import.eu"])
        [bare $ prop "result" $ normalName "foo"]
  describe "embedded lambda parsing" $ do
    it "parses '(x, y) x + y'" $
      testParse embeddedLambda "(x, y) x + y" `shouldParse`
      embLambda ["x", "y"] (opsoup [normalName "x", operatorName "+", normalName "y"])
    it "parses '   (x, y)    x + y   '" $
      testParse embeddedLambda "   (x, y)    x + y   " `shouldParse`
      embLambda ["x", "y"] (opsoup [normalName "x", operatorName "+", normalName "y"])
    it "parses '   (x, y)x+y'" $
      testParse embeddedLambda "   (x, y)    x + y   " `shouldParse`
      embLambda ["x", "y"] (opsoup [normalName "x", operatorName "+", normalName "y"])
