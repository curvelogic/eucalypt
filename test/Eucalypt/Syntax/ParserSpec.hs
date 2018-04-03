module Eucalypt.Syntax.ParserSpec (main, spec)
where

import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.Parser
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Modifiers
import Control.Monad
import Data.Either

main :: IO ()
main = hspec spec

checkNumber :: Integer -> Bool
checkNumber x = either (const False) (== VInt x) result
  where result = parseString parseNumber (show x)

checkFloat :: Double -> Bool
checkFloat x = either (const False) (== VFloat x) result
  where result = parseString parseNumber (show x)

checkSymbol :: String -> Bool
checkSymbol x = either (const False) (== VSym x) result
  where result = parseString parseSymbol (":" ++ x)

checkString :: String -> Bool
checkString x = either (const False) (== VStr x) result
  where result = parseString parseDQString ("\"" ++ x ++ "\"")

{- Generators for property testing -}

lower = ['a'..'z']
upper = ['A'..'Z']
digits = ['0'..'9']
idStartPunc = "$?!_"
idContPunc = "$?!_-*"
operatorStart = "@£%^&*|></+=-~"
operatorCont = "!@£$%^&*|></?+=-~"
identifierStart = lower ++ upper ++ digits ++ idStartPunc
identifierCont = lower ++ upper ++ digits ++ idContPunc

validIdentifierNames :: Gen String
validIdentifierNames = (:) <$> oneof (map return identifierStart)
  <*> listOf1 (oneof (map return identifierCont))

validSQStrings :: Gen String
validSQStrings = fmap (\x -> "'" ++ filter (/= '\'') x ++ "'") arbitrary

validOperatorNames :: Gen String
validOperatorNames = gen `suchThat` (/= "//")
  where gen = (:) <$> oneof (map return operatorStart) <*> listOf (oneof (map return operatorCont))

-- | Check for Right value which parses to simple name
checkNormalName :: String -> Bool
checkNormalName x = either (const False) (== NormalName x) result
  where result = parseString parseNormalName x


sampleA = "` { doc: \"This will be automatically called for dotted identifiers\" } \
\lookup(alist, sym): \
\  if((car(car(alist))=sym), \
\      cdr(car(alist)), \
\      lookup(cdr(alist), sym))"

sampleB = "reverse(list): if(car(list), \
\                  concat(reverse(list),[car(list)]), \
\                  nil)  "

spec :: Spec
spec = do

  describe "SimpleIdentifier" $ do
    it "parses xyz" $
      parseString parseNormalName "xyz" `shouldBe` Right (NormalName "xyz")

    it "rejects {{{" $
      parseString parseNormalName "{{{" `shouldSatisfy` isLeft

    it "parses 'singlequoted'" $
      parseString parseNormalName "'singlequoted'" `shouldBe` Right (NormalName "singlequoted")

    it "accepts legal identifiers" $
      forAll validIdentifierNames checkNormalName

    it "accepts almost anything singlequoted" $ property $
      forAll validSQStrings $ \qs -> parseString parseNormalName qs == Right (NormalName (take (length qs - 2) (tail qs)))

  describe "Identifier" $ do
    it "parses x.y.z" $
      parseString parseIdentifier "x.y.z" `shouldBe` Right (ident "x.y.z")

    it "parses 'x'.'y'.'z'" $
      parseString parseIdentifier "'x'.'y'.'z'" `shouldBe` Right (ident "'x'.'y'.'z'")

    it "parses '~'" $
      parseString parseIdentifier "'~'" `shouldBe` Right (ident "'~'")

  describe "Number" $
    it "accepts integers" $ property checkNumber

  describe "Float" $
    it "accepts floats" $ property checkFloat

  describe "Symbol" $
    it "accepts legal symbol names" $
      forAll validIdentifierNames checkSymbol

  describe "Operations" $ do
    it "accepts foo + bar" $
      parseAll parseOperation "foo + bar" `shouldBe` Right (op "+" (ident "foo") (ident "bar"))

    it "accepts foo & bar & baz" $
      parseAll parseOperation "foo & bar & baz" `shouldBe` Right (op "&"
                                                                     (op "&" (ident "foo") (ident "bar"))
                                                                     (ident "baz"))

    it "accepts x.y.z *^&%^ x.y.y" $
      parseAll parseOperation "x.y.z *^&%^ x.y.y" `shouldBe` Right (op "*^&%^" (ident "x.y.z") (ident "x.y.y"))

    it "accepts any valid op names" $
      forAll validOperatorNames $ \o -> parseAll parseOperation (" x " ++ o ++ " y ") == Right (op o (ident "x") (ident "y"))

  describe "Catenations" $
    it "accepts x y" $
      parseAll parseOperand "x y" `shouldBe` Right (cat (ident "x") (ident "y"))

  describe "Invocations" $ do
    it "parses f(x, y)" $
      parseAll parseInvocation " f(x, y) " `shouldBe` Right (invoke (ident "f") [ident "x", ident "y"])

    it "parses f(zz)" $
      parseAll parseInvocation " f(zz) " `shouldBe` Right (invoke (ident "f") [ident "zz"])

    it "parses foo.bar.baz(quux)" $
      parseAll parseInvocation " foo.bar.baz(quux) " `shouldBe` Right (invoke (ident "foo.bar.baz") [ident "quux"])

    it "parses foo.bar.'baz'(quux)" $
      parseAll parseInvocation " foo.bar.'baz'(quux) " `shouldBe` Right (invoke (ident "foo.bar.baz") [ident "quux"])

    it "parses if(foo(bar),baz,quux) " $
      parseAll parseInvocation " if(foo(bar),baz,quux)  " `shouldBe` Right (invoke (ident "if")
                                                                             [invoke (ident "foo") [ident  "bar"],
                                                                              ident "baz",
                                                                              ident "quux"])

    it "parses if(car(xs), concat(reverse(xs),[car(xs)]), nil)" $
      parseAll parseInvocation " if(car(xs), concat(reverse(xs),[car(xs)]), nil)"
        `shouldBe` Right (invoke (ident "if") [invoke (ident "car") [ident "xs"],
                                               invoke (ident "concat") [invoke (ident "reverse") [ident "xs"],
                                                                         list [invoke (ident "car") [ident "xs"]]],
                                               ident "nil"])

  describe "Expressions" $ do
    it "accepts x y" $
      parseAll parseExpression " x y " `shouldBe` Right (cat (ident "x") (ident "y"))

    it "parses x y |<| z" $
      parseAll parseExpression " x y |<| z " `shouldBe` Right (op "|<|" (cat (ident "x") (ident "y")) (ident "z"))

    it "parses g. h .'i'" $
      parseAll parseExpression " g. h .'i' " `shouldBe` Right (ident "g.h.i")

    it "parses 987" $
      parseAll parseExpression " 987 " `shouldBe` Right (ELiteral $ VInt 987)

    it "parses f(x, y)" $
      parseAll parseExpression " f(x, y) " `shouldBe` Right (invoke (ident "f") [ident "x", ident "y"])

    it "parses x y z" $
      parseAll parseExpression " x y z " `shouldBe` Right (cat (cat (ident "x") (ident "y")) (ident "z"))

    it "parses y (x f)" $
      parseAll parseExpression " y (x f) " `shouldBe` Right (cat (ident "y") (cat (ident "x") (ident "f")))

    it "parses y f(x)" $
      parseAll parseExpression " y f(x) " `shouldBe` Right (cat (ident "y") (invoke (ident "f") [ident "x"]))

    it "parses y (f(x))" $
      parseAll parseExpression " y (f(x)) " `shouldBe` Right (cat (ident "y") (invoke (ident "f") [ident "x"]))

    it "takes some of x y :" $
      parseString parseExpression " x y : " `shouldBe` Right (ident "x")

  describe "Simple expression" $ do
    it "fails x :" $
      parseAll parseSimpleExpression " x : " `shouldSatisfy` isLeft

    it "fails h(y) :" $
      parseAll parseSimpleExpression " h(y) : " `shouldSatisfy` isLeft

    it "fails (gg++yy) :" $
      parseAll parseSimpleExpression " (gg++yy) : " `shouldSatisfy` isLeft

  describe "Lists" $
    it "parses [a, b, c]" $
      parseAll parseList " [a, b, c] " `shouldBe` Right (list [ident "a", ident "b", ident "c"])

  describe "Property declarations" $ do
    it "parses x: y" $
      parseAll parsePropertyDecl " x: y " `shouldBe` Right (prop "x" (ident "y"))

    it "parses f(x,y) : y" $
      parseAll parseFunctionDecl " f(x,y) : y " `shouldBe` Right (func "f" ["x", "y"] (ident "y"))

    it "parses (foo *^^* bar) : bar foo(344)" $
      parseAll parseOperatorDecl " (foo *^^* bar) : bar foo(344) " `shouldBe` Right (oper "*^^*" "foo" "bar"
                                                                                      (cat
                                                                                        (ident "bar")
                                                                                        (invoke (ident "foo") [int 344])))

  describe "Properties" $ do
    it "parses ` a x : y " $
      parseAll parseProperty " ` a x : y " `shouldBe` (Right $ ann (ident "a") (prop "x" (ident "y")))

    it "parses ` a b c zz(yy,gg) : y " $
      parseAll parseProperty " ` a b c zz(yy,gg) : yy " `shouldBe` (Right $ ann (cat (cat (ident "a") (ident "b")) (ident "c"))
                                                                                (func "zz" ["yy", "gg"] (ident "yy")))

    it "parses ` a (foo + bar) : baz " $
      parseAll parseProperty " ` a (foo + bar) : baz " `shouldBe` (Right $ ann (ident "a")
                                                                               (oper "+" "foo" "bar" (ident "baz")))

    it "parses ` { if: :listfn } lists: { } " $
      parseAll parseProperty " ` { if: :listfn } \n lists: { } " `shouldBe` (Right $ ann (block [bare (prop "if" (sym "listfn"))])
                                                                                         (prop "lists" (block [])))

    it "parses items: [\"one\", \"two\", \"three\"]" $
      parseAll parseProperty " items: [\"one\", \"two\", \"three\"] " `shouldBe`
      Right (bare (prop "items" (list [str "one", str "two", str "three"])))

  describe "Unit" $
    it "parses foo : bar baz : quux " $
      parseAll parseUnit " foo : bar baz : quux " `shouldBe` (Right $ Block $ map bare [prop "foo" (ident "bar"),
                                                                                        prop "baz" (ident "quux")])
      
  describe "Block literal" $ do
    it "parses { `a x : y }" $
      parseAll parseBlock " { `a x : y } " `shouldBe` (Right $ Block [ann (ident "a") (prop "x" (ident "y"))])
 

    it "parses { `{ z:g } x : y }" $
      parseAll parseBlock " { `{ z:g } x : y } " `shouldBe` (Right $ Block [ann
                                                                             (block [bare (prop "z" (ident "g"))])
                                                                             (prop "x" (ident "y"))])

  describe "Samples" $ do
    it "parses sample A" $
      parseAll parseUnit sampleA
      `shouldBe`
      (Right $ Block [ann
                      (block [bare (prop "doc" (str "This will be automatically called for dotted identifiers"))])
                      (func

                      -- function name
                       "lookup"

                       -- args
                       ["alist", "sym"]

                       -- body 
                       (invoke (ident "if")
                        [op "="
                          (invoke (ident "car") [invoke (ident "car") [ident "alist"]])
                          (ident "sym"),
                         invoke (ident "cdr") [invoke (ident "car") [ident "alist"]],
                         invoke (ident "lookup") [invoke (ident "cdr") [ident "alist"], ident "sym"]]))])

    it "parses sample B" $
      parseAll parseUnit sampleB
      `shouldBe`
      (Right $ Block [bare (func
                             "reverse"
                             ["list"]
                             (invoke (ident "if")
                              [invoke (ident "car") [ident "list"],
                               invoke (ident "concat")
                                        [invoke (ident "reverse") [ident "list"],
                                         list [invoke (ident "car") [ident "list"]]],
                               ident "nil"]))])


  describe "Nested metadata" $
    it "parses nested metadata" $
      parseAll parseBlock " { `{`{c:d}a:b} x : y } "
      `shouldBe`
      (Right $ Block [ann
                      (block [ann
                               (block [bare (prop "c" (ident "d"))])
                               (prop "a" (ident "b"))])
                      (prop "x" (ident "y"))])

