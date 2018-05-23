{-|
Module      : Eucalypt.Syntax.ParseExpr
Description : Parser for the eucalypt language
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Syntax.ParseExpr where

import Data.Char (isAscii, isSymbol)
import Data.Void (Void)
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String

-- | Whitespace parser also discards line comments for now
sc :: Parser ()
sc = L.space space1 lineComment empty
  where
    lineComment = L.skipLineComment "#"


-- | In some areas, we don't consume whitespace
nosc :: Parser ()
nosc = L.space empty empty empty


-- | A lexeme parser cannot be interpreted as a function to be applied
-- and so consumes trailing whitespace
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- $locations
--
-- Most elements of the expression tree are annotated with source
-- location, these combinators record or alter the locations.

-- | Takes a parser and records source locations to parse a located
-- version
located :: Parser a -> Parser (Located a)
located p = (\s x e -> at (mpos s, mpos e) x) <$> getPosition <*> p <*> getPosition

-- | Takes a parser which already records location and updates the
-- location
relocated :: Parser (Located a) -> Parser (Located a)
relocated p =  (\s x e -> move (mpos s, mpos e) x) <$> getPosition <*> p <*> getPosition


-- | A normal (non-operator) identifier, n.b. leading digits are legal
-- in Eucalypt: @5v@ is an identifier.
normalIdentifier :: Parser String
normalIdentifier =
  ((:) <$> normalIdentStartChar <*> many normalIdentContinuationChar) <?>
  "normal identifier"
  where
    normalIdentStartChar = alphaNumChar <|> oneOf "$?_"
    normalIdentContinuationChar = alphaNumChar <|> oneOf "$?!_-*"



-- | An operator identifier
operatorIdentifier :: Parser String
operatorIdentifier =
  ((:) <$> opIdentStartChar <*> many opIdentContinuationChar) <?>
  "operator identifier"
  where
    opIdentStartChar =
      oneOf ".!@£%^&*|></+=-~" <|>
      satisfy (\c -> (not . isAscii) c && isSymbol c)
    opIdentContinuationChar =
      oneOf ".!@£$%^&*|></?+=-~" <|>
      satisfy (\c -> (not . isAscii) c && isSymbol c)



-- | A single quoted identifier.
--
-- There is *no escaping* in quoted identifiers. Put the character in
--
quotedIdentifier :: Parser String
quotedIdentifier =
  (char '\'' >> manyTill anyChar (char '\'')) <?> "single quoted identifier"


-- | Any type of identifier
identifier :: Parser String
identifier =
  try (normalIdentifier <|>
       operatorIdentifier <|>
       quotedIdentifier) <?>
  "identifier"



-- | Any type of identifier
categorisedIdentifier :: Parser AtomicName
categorisedIdentifier =
  try (NormalName <$> normalIdentifier <|>
       OperatorName <$> operatorIdentifier <|>
       NormalName <$> quotedIdentifier) <?>
  "identifier"



-- | An identifier as an expression
name :: Parser Expression
name = located (EName <$> categorisedIdentifier <?> "name")



-- ? primitives
--
-- We treat integers and doubles separately despite their being
-- unified in the syntax



integer :: Parser PrimitiveLiteral
integer = VInt <$> L.signed nosc L.decimal



float :: Parser PrimitiveLiteral
float = VFloat <$> L.signed nosc L.float



number :: Parser PrimitiveLiteral
number = try float <|> integer



stringLiteral :: Parser PrimitiveLiteral
stringLiteral = VStr <$> (char '"' >> manyTill L.charLiteral (char '"'))



symbolLiteral :: Parser PrimitiveLiteral
symbolLiteral = VSym <$> (char ':' >> identifier)


primitiveLiteral :: Parser PrimitiveLiteral
primitiveLiteral = try stringLiteral <|> symbolLiteral <|> number                 


primitive :: Parser Expression                   
primitive = located (ELiteral <$> primitiveLiteral <?> "primitive")

            
-- ? expression
--
--

atom :: Parser Expression
atom = try primitive <|> name
                         
-- ? calls
-- 

symbol :: String -> Parser String
symbol = L.symbol sc

comma :: Parser String
comma = symbol ","
         
parens :: Parser a -> Parser a         
parens = between (symbol "(") (char ')')

tuple :: Parser [Expression]
tuple = parens $ expression `sepBy1` comma

-- ? calls
--
        
rootCall :: Parser Expression
rootCall = located ((EInvocation <$> atomOrParenExp <*> tuple) <?> "function call")

invocation :: Expression -> [Expression] -> Expression
invocation f xs = at s $ EInvocation f xs
  where s = foldl1 merge (map location xs)
        
call :: Parser Expression
call = foldl invocation <$> rootCall <*> many tuple
       
atomOrParenExp :: Parser Expression
atomOrParenExp = try atom <|> parenExpression

element :: Parser Expression
element =
  (try call <|> atom <|> listLiteral <|> parenExpression) <?>
  "element"

-- ? lists
--

squares :: Parser a -> Parser a         
squares = between (symbol "[") (char ']')

listLiteral :: Parser Expression
listLiteral =
  located (EList <$> squares (expression `sepBy1` comma) <?> "list literal")

-- ? expressions

unparenExpression :: Parser Expression
unparenExpression = located $ do
  exprs <- element `sepBy1` space1
  return $ case exprs of
    [e] -> locatee e
    es -> EOpSoup implicit es
    
parenExpression :: Parser Expression
parenExpression = located $ do
  exprs <- parens (element `sepBy1` space1)
  return $ case exprs of
    [e] -> locatee e
    es -> EOpSoup parentheses es

expression :: Parser Expression
expression = try unparenExpression <|> parenExpression

