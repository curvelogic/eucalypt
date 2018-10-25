{-|
Module      : Eucalypt.Syntax.ParseExpr
Description : Parser for the eucalypt language
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Syntax.ParseExpr where

import Data.Bifunctor (first)
import Data.Char (isPunctuation, isAscii, isSymbol)
import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.Error
import Eucalypt.Syntax.ParseCommon
import Eucalypt.Syntax.ParseString
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

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



-- | An operator identifier
operatorIdentifier :: Parser String
operatorIdentifier =
  ((:) <$> opIdentStartChar <*> many opIdentContinuationChar) <?>
  "operator identifier"
  where
    opIdentStartChar =
      oneOf ".!@£%^&*|></+=-~" <|>
      satisfy (\c -> (not . isAscii) c && (isSymbol c || isPunctuation c))
    opIdentContinuationChar =
      oneOf ".!@£$%^&*|></?+=-~" <|>
      satisfy (\c -> (not . isAscii) c && (isSymbol c || isPunctuation c))



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


-- | Any type of identifier
categorisedNormalIdentifier :: Parser AtomicName
categorisedNormalIdentifier =
  try (NormalName <$> normalIdentifier <|>
       NormalName <$> quotedIdentifier) <?>
  "identifier"


-- | An identifier as an expression
name :: Parser Expression
name = located (EName <$> categorisedIdentifier <?> "name")



-- | A non-operator identifier as an expression
nonOperatorName :: Parser Expression
nonOperatorName = located (EName <$> categorisedNormalIdentifier <?> "property name")



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


symbolLiteral :: Parser PrimitiveLiteral
symbolLiteral = VSym <$> (char ':' >> identifier)


primitiveLiteral :: Parser Expression
primitiveLiteral = located $ ELiteral <$> (try symbolLiteral <|> number)


stringLiteral :: Parser Expression
stringLiteral = located $ do
  chunks <- char '"' *> quotedStringContent <* char '"'
  return $ case chunks of
    [] -> ELiteral $ VStr ""
    [LiteralContent s] -> ELiteral $ VStr s
    xs -> EStringPattern xs


primitive :: Parser Expression                   
primitive =
  try (stringLiteral <?> "string literal") <|>
  (primitiveLiteral <?> "primitive")

            
-- ? expression
--
--

atom :: Parser Expression
atom = try primitive <|> name
                         
-- ? calls
-- 

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
--
-- Also note that instances of generalised lookup may have parentheses
-- directly after a dot operator, e.g. @{a: 1}.(a+1)@. These must not
-- be parsed as calls.
rootCall :: Parser [Expression]
rootCall = label "function call" $
           (\x y -> [x, y]) <$> callAnchorExpression <*> located (EApplyTuple <$> tuple)

invocation :: [Expression] -> [Expression] -> [Expression]
invocation es xs = es ++ [at s $ EApplyTuple xs]
  where s = foldl1 merge (map location xs)
        
call :: Parser [Expression]
call = foldl invocation <$> rootCall <*> many tuple
       
callAnchorExpression :: Parser Expression
callAnchorExpression = try nonOperatorName <|> parenExpression

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
  located (EList <$> squares (expression `sepBy` comma) <?> "list literal")

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

prefixOperatorSignature :: Parser (AtomicName, String)
prefixOperatorSignature = do
  o <- symbol "(" >> (OperatorName <$> lexeme operatorIdentifier)
  x <- lexeme normalIdentifier <* symbol ")"
  return (o, x)

postfixOperatorSignature :: Parser (String, AtomicName)
postfixOperatorSignature = do
  x <- symbol "(" >> lexeme normalIdentifier
  o <- (OperatorName <$> lexeme operatorIdentifier) <* symbol ")"
  return (x, o)

operatorDeclaration :: Parser DeclarationForm
operatorDeclaration =
  label "operator declaration" $
  lexeme $
  located $ do
    (l, o, r) <- operatorSignature
    expr <- colon >> expression
    return $ OperatorDecl o l r expr

prefixOperatorDeclaration :: Parser DeclarationForm
prefixOperatorDeclaration =
  label "prefix operator declaration" $
  lexeme $
  located $ do
    (o, x) <- prefixOperatorSignature
    expr <- colon >> expression
    return $ LeftOperatorDecl o x expr

postfixOperatorDeclaration :: Parser DeclarationForm
postfixOperatorDeclaration =
  label "postfix operator declaration" $
  lexeme $
  located $ do
    (x, o) <- postfixOperatorSignature
    expr <- colon >> expression
    return $ RightOperatorDecl o x expr

declarationForm :: Parser DeclarationForm
declarationForm =
  lexeme $
  try propertyDeclaration <|> try functionDeclaration <|> try operatorDeclaration <|>
  try prefixOperatorDeclaration <|> postfixOperatorDeclaration
  
declarationAnnotation :: Parser Expression
declarationAnnotation =
  (symbol "`" >> lexeme expression) <?> "declaration annotation"

anyDeclaration :: Parser BlockElement
anyDeclaration = label "declaration" $ lexeme $ located $ do
  a <- optional declarationAnnotation
  d <- declarationForm
  return $ Declaration Annotated { annotation = a, content = d }

blockContent :: Parser Block
blockContent = sc >> located (Block <$> many anyDeclaration) <?> "block content"

blockLiteral :: Parser Expression
blockLiteral = located $ EBlock <$> braces blockContent

unannotatedBlock :: Parser (Annotated Block)
unannotatedBlock = (Annotated Nothing <$> blockContent) <?> "unannotated block"

annotatedBlock :: Parser (Annotated Block)
annotatedBlock =
  (Annotated <$> (Just <$> lexeme expression) <*> blockContent) <?>
  "annotated block"

unit :: Parser (Located (Annotated Block))
unit = sc >> located (try $ unannotatedBlock <* eof <|> annotatedBlock <* eof)

-- ? driver functions
--

parseUnit :: String -> String -> Either SyntaxError (Located (Annotated Block))
parseUnit text n  = first MegaparsecError $ parse unit n text

parseExpression :: String -> String -> Either SyntaxError Expression
parseExpression text n = first MegaparsecError $ parse expression n text
