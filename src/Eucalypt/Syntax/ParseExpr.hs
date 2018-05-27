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



-- | An identifier that can be used as the name of a property of
-- function declaration
propertyIdentifier :: Parser AtomicName
propertyIdentifier = NormalName <$> lexeme (try normalIdentifier <|> quotedIdentifier) <?> "property identifier"



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
-- Function calls (that are not achieved by catenation) are identified
-- by placing the argument tuple directly against an 'anchor'
-- expression. e.g. @f.g.h("z")@. As qualified names are parsed as
-- expressions, the anchor expression ("h") may not be ultimately the
-- function called, depending on operator precedence, however, the
-- call parser identifies both the anchor and the arg tuple and
-- returns both to the expression parser for inclusion in opsoup. This
-- is why 'element' returns more than one expression.
-- 
rootCall :: Parser [Expression]
rootCall = label "function call" $
           (\x y -> [x, y]) <$> callAnchorExpression <*> located (EApplyTuple <$> tuple)

invocation :: [Expression] -> [Expression] -> [Expression]
invocation es xs = es ++ [at s $ EApplyTuple xs]
  where s = foldl1 merge (map location xs)
        
call :: Parser [Expression]
call = foldl invocation <$> rootCall <*> many tuple
       
callAnchorExpression :: Parser Expression
callAnchorExpression = try atom <|> parenExpression

listify :: Parser a -> Parser [a]
listify = fmap (: [])

element :: Parser [Expression]
element =
  label
    "element"
    (try call <|> listify atom <|> listify listLiteral <|>
     listify parenExpression <|>
     listify blockLiteral)


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
  exprs <- concat <$> freeElement `sepEndBy1` sc
  return $ case exprs of
    [e] -> locatee e
    es -> EOpSoup implicit es
  where freeElement = try (element <* notFollowedBy (sc >> char ':' >> space1))
    
parenExpression :: Parser Expression
parenExpression = located $ do
  exprs <- concat <$> parens (element `sepEndBy1` sc)
  return $ case exprs of
    [e] -> locatee e
    es -> EOpSoup parentheses es

expression :: Parser Expression
expression = (try unparenExpression <|> parenExpression) <?> "expression"

-- ? blocks
--
-- Most of the block related parsers are lexeme parsers as whitespace
-- is not significant. Block itself is an exception as it is callable
-- and a block literal could therefore be used in invocation syntax.

colon :: Parser String
colon = symbol ":"

propertyDeclaration :: Parser DeclarationForm
propertyDeclaration =
  label "property declaration" $ lexeme $ located
    (PropertyDecl <$> propertyIdentifier <* colon <*>
     lexeme expression <?> "property declaration")
  
parenTuple :: Parser [String]
parenTuple = parens $ normalIdentifier `sepBy1` comma

functionDeclaration :: Parser DeclarationForm
functionDeclaration =
  label "function declaration" $ lexeme $ located $
  FunctionDecl <$> propertyIdentifier <*> lexeme parenTuple <*> (colon >> expression)

operatorSignature :: Parser (String, AtomicName, String)
operatorSignature = do
  l <- symbol "(" >> lexeme normalIdentifier
  o <- OperatorName <$> lexeme operatorIdentifier
  r <- lexeme normalIdentifier <* symbol ")"
  return (l, o, r)

operatorDeclaration :: Parser DeclarationForm
operatorDeclaration = label "operator declaration" $ lexeme $ located $ do
  (l, o, r) <- operatorSignature
  expr <- colon >> expression
  return $ OperatorDecl o l r expr

declarationForm :: Parser DeclarationForm
declarationForm =
  lexeme $
  try propertyDeclaration <|> functionDeclaration <|> operatorDeclaration
  
declarationAnnotation :: Parser Expression
declarationAnnotation =
  (symbol "`" >> lexeme expression) <?> "declaration annotation"

anyDeclaration :: Parser BlockElement
anyDeclaration = label "declaration" $ lexeme $ located $ do
  a <- optional declarationAnnotation
  d <- declarationForm
  return $ Declaration Annotated { annotation = a, declaration = d }

blockContent :: Parser Block
blockContent = sc >> located (Block <$> many anyDeclaration)

braces :: Parser a -> Parser a         
braces = between (symbol "{") (char '}')

blockLiteral :: Parser Expression
blockLiteral = located $ EBlock <$> braces blockContent

unit :: Parser Expression
unit = located $ EBlock <$> blockContent
