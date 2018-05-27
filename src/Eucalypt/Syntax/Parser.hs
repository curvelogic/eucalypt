{-|
Module      : Eucalypt.Syntax.Parser
Description : Parser for the eucalypt language
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Eucalypt.Syntax.Parser where

import Eucalypt.Reporting.Location
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.Error
import Text.Parsec (try, parse, getPosition)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Prim (many)
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as Tok
import Data.Char (isAscii, isSymbol)
import Data.Bifunctor
import Control.Applicative hiding (many)

-- $lexer
--
--

langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = ""
  , Tok.commentEnd      = ""
  , Tok.commentLine     = "#"
  , Tok.nestedComments  = False
  , Tok.identStart      = alphaNum <|> oneOf "$?_"
  , Tok.identLetter     = alphaNum <|> oneOf "$?!_-*"
  , Tok.opStart         = oneOf "!@£%^&*|></+=-~" <|> satisfy (\c -> (not . isAscii) c && isSymbol c)
  , Tok.opLetter        = oneOf "!@£$%^&*|></?+=-~" <|> satisfy (\c -> (not . isAscii) c && isSymbol c)
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = ["`", "."]
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

identStart = Tok.identStart langDef
identLetter = Tok.identLetter langDef

opStart = Tok.opStart langDef
opLetter = Tok.opLetter langDef

number = Tok.naturalOrFloat lexer
lexeme = Tok.lexeme lexer
identifier = Tok.identifier lexer
dot = Tok.dot lexer
comma = Tok.comma lexer
colon = Tok.colon lexer
parens = Tok.parens lexer
squares = Tok.squares lexer
operator = Tok.operator lexer
whiteSpace = Tok.whiteSpace lexer
braces = Tok.braces lexer

-- $locations
--
-- Most elements of the expression tree are annotated with source
-- location, these combinators record or alter the locations.

-- | Takes a parser and records source locations to parse a located
-- version
located :: Parser a -> Parser (Located a)
located p = (\s x e -> at (spos s, spos e) x) <$> getPosition <*> p <*> getPosition

-- | Takes a parser which already records location and updates the
-- location
relocated :: Parser (Located a) -> Parser (Located a)
relocated p =  (\s x e -> move (spos s, spos e) x) <$> getPosition <*> p <*> getPosition

-- $identifiers
--
-- Broadly speaking there are two types of identifiers, normal names
-- and operator names which are distinguished by the types of
-- character they can contain. We do not (any longer) use a concept of
-- qualified names, qualified names (a.b.c) are treated as the use of
-- a high precedence, left associative dot operator and just handled as
-- lookups.
--
-- This leads to a few complicated cases we rule out after the parse:
-- * a.+.b (operator names can only be the last )
-- * a.b.(c+d) ("generalised lookup" which may be handled by late
--   binding later on but isn't handled for now
--
-- Single quotes may be used to allow the use of special characters
-- within a normal name.
--
-- Normal names:
-- * x (atomic name)
-- * x.y.z (qualified name, i.e. lookup of z in y in x)
-- * _x.$y.?z (qualified name)
-- * '<>' (atomic name)
-- * x.y.z.'~' (qualified name)
--
-- Operator names:
-- * <> (atomic operator name)
-- * <$$--* (atomic operator name)
-- * x.<> (qualified operator name)
-- * x.y.<$$> (qualified operator name)
-- * '*'.'*'.< (qualified operator name)
--
-- Open parens appearing directly after a lexical element with __no
-- intervening whitespace__ indicate a function call, e.g.
-- @x.y.z(a)@ or @f(a)(b)(c)@ or @(flip f)(c)@
--
-- This means we need versions of the identifier parsers that don't
-- consume trailing whitespace. Parsec doesn't seem to make this easy.

-- | Parse contents surrounded by quote char and apply data constructor
parseQuoted :: Char -> (String -> a) -> Parser a
parseQuoted q con =
  fmap con (char q >> manyTill anyChar (try (char q)))

-- | Parse a single quoted name
parseSQStringNoWS :: Parser AtomicName
parseSQStringNoWS = parseQuoted '\'' NormalName

-- | Parse a single quoted name, allowing trailing whitespace
parseSQString :: Parser AtomicName
parseSQString = lexeme $ parseQuoted '\'' NormalName

-- | Parse atomic name with no special characters
parseNormalName :: Parser AtomicName
parseNormalName = parseSQString <|> NormalName <$> identifier

-- | Parameters are more restricted
parseParameterName :: Parser ParameterName
parseParameterName = identifier

-- | Parse just a simple name with no special characters
parseNormalNameNoWS :: Parser AtomicName
parseNormalNameNoWS = do
  x <- identStart
  xs <- many identLetter
  return $ NormalName (x:xs)

-- | Parse a simple operator
parseOperatorName :: Parser AtomicName
parseOperatorName = OperatorName <$> operator

-- | Parse a simple operator with no trailing WS
parseOperatorNameNoWS :: Parser AtomicName
parseOperatorNameNoWS = do
  x <- opStart
  xs <- many opLetter
  return $ OperatorName (x:xs)

-- | Parse either atomic name type
parseAtomicName :: Parser AtomicName
parseAtomicName = parseNormalName <|> parseOperatorName

-- | Parse either atomic name type with no trailing WS
parseAtomicNameNoWS :: Parser AtomicName
parseAtomicNameNoWS = parseNormalNameNoWS <|> parseOperatorNameNoWS

-- | Parse a qualified normal identifier
parseNormalIdentifier :: Parser Expression
parseNormalIdentifier = located $ EIdentifier <$> parseNormalName `sepBy1` dot

-- | Parse a normal identifier (not operator) with no trailing
-- whitespace. Operator names are not used in function call syntax so
-- we don't need to parse that case.
parseNormalIdentifierNoWS :: Parser Expression
parseNormalIdentifierNoWS = located $ do
  prefixes <- many (try (parseNormalName <* dot))
  base <- try parseSQStringNoWS <|> parseNormalNameNoWS
  return $ EIdentifier $ prefixes ++ [base]

-- | Parse a general identifer, qualified or simple, normal or operator
parseIdentifier :: Parser Expression
parseIdentifier = located $ EIdentifier <$> parseAtomicName `sepBy1` dot

-- | Parse a general normal or operator identifer, qualified or
-- simple, with no whitespace
parseIdentifierNoWS :: Parser Expression
parseIdentifierNoWS = located $ do
  prefixes <- many (try (parseNormalName <* dot))
  base <- try parseSQStringNoWS <|> parseAtomicNameNoWS
  return $ EIdentifier $ prefixes ++ [base]


-- $primitives
--
-- The following literal types are primitives:
--
-- * number (integer or float)
-- * symbol
-- * string

-- | Parse either an integer or a float.
parseNumber :: Parser PrimitiveLiteral
parseNumber = do
  neg <- (oneOf "+-" >>= (\c -> return $ c == '-')) <|> return False
  iOrF <- number
  case iOrF of
    Left i -> return $ VInt ((if neg then negate else id) i)
    Right f -> return $ VFloat ((if neg then negate else id) f)

-- | Parse symbol (a colon-prefixed simple name).
parseSymbol :: Parser PrimitiveLiteral
parseSymbol = fmap VSym (char ':' *> Tok.identifier lexer)

-- | Parse double quoted string
parseDQString :: Parser PrimitiveLiteral
parseDQString = lexeme $ parseQuoted '"' VStr

-- | Parse a literal
parseLiteral :: Parser Expression
parseLiteral = located $ ELiteral <$> (try parseNumber <|> try parseSymbol <|> try parseDQString)

-- $expressions
--
-- Catenations are represented explicitly (and transformed into
-- function calls in the core syntax).
--
-- Operator precedence is complicated and ignored at this stage and
-- post-processed prior to translation to core.
--

catenate :: Expression -> Expression -> Expression
catenate l@Located {location = locl} r@Located {location = locr} =
  at (merge locl locr) $ ECatenation l r

parseOperand :: Parser Expression
parseOperand = foldl1 catenate <$> many1 parseSimpleExpression

operation :: AtomicName -> Expression -> Expression -> Expression
operation n l@Located {location = locl} r@Located {location = locr} =
  at (merge locl locr) $ EOperation n l r

parseOperator :: Parser (Expression -> Expression -> Expression)
parseOperator = operation . OperatorName <$> operator

parseOperation :: Parser Expression
parseOperation = parseOperand `chainl1` parseOperator

parseExpression :: Parser Expression
parseExpression = try parseOperation <|> parseOperand

-- | Parse a direct invocation e.g. `f(x, y)` with no whitespace
-- between the function and argument list
parseInvocation :: Parser Expression
parseInvocation = located $ do
  fn <- try parseIdentifierNoWS
  exprs <- parens $ parseExpression `sepBy1` comma
  return $ EInvocation fn exprs

-- | Parse any expression in brackets
parseParenExpression :: Parser Expression
parseParenExpression = parens parseExpression

-- | Parse a list literal
parseList :: Parser Expression
parseList = located $ EList <$> squares ( parseExpression `sepBy` comma)

-- | A simple expression is not an operation or a catenation.
parseSimpleExpression :: Parser Expression
parseSimpleExpression = try $ do
  expr <- try parseList <|> try parseParenExpression <|> try parseBlockLiteral <|> try parseInvocation <|> parseLiteral <|> parseNormalIdentifier
  notFollowedBy colon
  return expr

-- $blocks
--

parsePropertyDecl :: Parser DeclarationForm
parsePropertyDecl = located $ PropertyDecl <$> parseNormalName  <* colon <*> parseExpression

parseFunctionDecl :: Parser DeclarationForm
parseFunctionDecl = located $ do
  fn <- parseNormalNameNoWS
  args <- parens $ parseParameterName `sepBy1` comma
  _ <- colon
  expr <- parseExpression
  return $ FunctionDecl fn args expr

parseOperatorDecl :: Parser DeclarationForm
parseOperatorDecl = located $ do
  signature <- parens parseItems
  _ <- colon
  expr <- parseExpression
  return $ signature expr
  where parseItems = do
          lhs <- parseParameterName
          opName <- operator
          rhs <- parseParameterName
          return $ OperatorDecl (OperatorName opName) lhs rhs

parseDecl :: Parser DeclarationForm
parseDecl = try parseOperatorDecl <|> try parsePropertyDecl <|> parseFunctionDecl

parseAnnotation :: Parser Expression
parseAnnotation = lexeme (char '`') >> parseExpression

parseProperty :: Parser BlockElement
parseProperty = located $ do
  a <- optionMaybe parseAnnotation
  d <- parseDecl
  return $ Declaration Annotated { annotation = a, declaration = d }

-- |
-- Parse top level declarations as block but allow any amount of preceding whitespace
parseUnit :: Parser Block
parseUnit = whiteSpace >> located (Block <$> many parseProperty)

parseTopLevel :: Parser Expression
parseTopLevel = located $ EBlock <$> parseUnit

parseBlock :: Parser Block
parseBlock = relocated $ braces parseUnit

parseBlockLiteral :: Parser Expression
parseBlockLiteral = located $ EBlock <$> parseBlock

-- $utilities
--
--



-- | Allow leading whitespace
parseSource :: Parser a -> Parser a
parseSource p = whiteSpace >> p



-- | Parse a Eucalypt toplevel unit from string potentially leaving
-- residue
parseString :: Parser a -> String -> Either SyntaxError a
parseString p s = first SyntaxError $ parse (parseSource p) "" s


-- | Parse the entirety of a named input as a Eucalypt toplevel unit
--
parseNamedInput :: Parser a -> String -> String -> Either SyntaxError a
parseNamedInput p n s = first SyntaxError $ parse (parseSource p <* eof) n s


-- | Parse unnamed text
parseAll :: Parser a -> String -> Either SyntaxError a
parseAll p = parseNamedInput p ""
