{-|
Module      : Eucalypt.Syntax.Parser
Description : Parser for the eucalypt language
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Syntax.Parser where

import Eucalypt.Syntax.Ast
import Text.Parsec (try, ParseError, parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char
import Text.Parsec.Prim (many)
import Text.Parsec.Combinator
import qualified Text.Parsec.Token as Tok

import Data.Either (either)
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
  , Tok.identStart      = alphaNum <|> oneOf "$?!_"
  , Tok.identLetter     = alphaNum <|> oneOf "$?!_-*"
  , Tok.opStart         = oneOf "@£%^&*|></+=-~"
  , Tok.opLetter        = oneOf "!@£$%^&*|></?+=-~"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = ["`"]
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

-- $identifiers
--
-- Broadly speaking there are two types of identifiers, normal names
-- and operator names. All names may be qualified. The final name
-- component determines whether the name is normal or an operator
-- name. For normal names, all name components are simple names. For
-- operator names, all leading components are simple names but the
-- final component is an operator name, distinguished by the set of
-- characters used.
--
-- Single quotes may be used to allow the use of special characters
-- within a normal name.
--
-- Normal names:
-- * x (atomic name)
-- * x.y.z (qualified name)
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
-- Normal names may appear directly next to open parens with __no
-- intervening whitespace__ to indicate a function call, e.g.
-- @x.y.z(a)@.
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
parseNormalIdentifier = EIdentifier <$> parseNormalName `sepBy1` dot

-- | Parse a normal identifier (not operator) with no trailing
-- whitespace. Operator names are not used in function call syntax so
-- we don't need to parse that case.
parseNormalIdentifierNoWS :: Parser Expression
parseNormalIdentifierNoWS = do
  prefixes <- many (try (parseNormalName <* dot))
  base <- try parseSQStringNoWS <|> parseNormalNameNoWS
  return $ EIdentifier $ prefixes ++ [base]

-- | Parse a general identifer, qualified or simple, normal or operator
parseIdentifier :: Parser Expression
parseIdentifier = EIdentifier <$> parseAtomicName `sepBy1` dot

-- | Parse a general normal or operator identifer, qualified or
-- simple, with no whitespace
parseIdentifierNoWS :: Parser Expression
parseIdentifierNoWS = do
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
parseLiteral = ELiteral <$> (try parseNumber <|> try parseSymbol <|> try parseDQString)

-- $expressions
--
-- Catenations are represented explicitly (and transformed into
-- function calls in the core syntax).
--
-- Operator precedence is complicated and ignored at this stage and
-- post-processed prior to translation to core.
--

parseOperand :: Parser Expression
parseOperand = foldl1 ECatenation <$> many1 parseSimpleExpression

parseOperator :: Parser (Expression -> Expression ->  Expression)
parseOperator = EOperation . OperatorName <$> operator

parseOperation :: Parser Expression
parseOperation = parseOperand `chainl1` parseOperator

parseExpression :: Parser Expression
parseExpression = try parseOperation <|> parseOperand

-- | Parse a direct invocation e.g. `f(x, y)` with no whitespace
-- between the function and argument list
parseInvocation :: Parser Expression
parseInvocation = do
  fn <- try parseIdentifierNoWS
  exprs <- parens $ parseExpression `sepBy1` comma
  return $ EInvocation fn exprs

-- | Parse any expression in brackets
parseParenExpression :: Parser Expression
parseParenExpression = parens parseExpression

-- | Parse a list literal
parseList :: Parser Expression
parseList = EList <$> squares ( parseExpression `sepBy` comma)

-- | A simple expression is not an operation or a catenation.
parseSimpleExpression :: Parser Expression
parseSimpleExpression = try $ do
  expr <- try parseList <|> try parseParenExpression <|> try parseBlockLiteral <|> try parseInvocation <|> parseLiteral <|> parseNormalIdentifier
  notFollowedBy colon
  return expr

-- $blocks
--

parsePropertyDecl :: Parser DeclarationForm
parsePropertyDecl = PropertyDecl <$> parseNormalName  <* colon <*> parseExpression

parseFunctionDecl :: Parser DeclarationForm
parseFunctionDecl = do
  fn <- parseNormalNameNoWS
  args <- parens $ parseNormalName `sepBy1` comma
  colon
  expr <- parseExpression
  return $ FunctionDecl fn args expr

parseOperatorDecl :: Parser DeclarationForm
parseOperatorDecl = do
  op <- parens parseItems
  colon
  expr <- parseExpression
  return $ op expr
  where parseItems = do
          lhs <- parseNormalName
          op <- operator
          rhs <- parseNormalName
          return $ OperatorDecl (OperatorName op) lhs rhs

parseDecl :: Parser DeclarationForm
parseDecl = try parseOperatorDecl <|> try parsePropertyDecl <|> parseFunctionDecl

parseAnnotation :: Parser Expression
parseAnnotation = lexeme (char '`') >> parseExpression

parseProperty :: Parser BlockElement
parseProperty = do
  annotation <- optionMaybe parseAnnotation
  decl <- parseDecl
  return $ Declaration Annotated { annotation = annotation, declaration = decl }

-- |
-- Parse top level declarations as block but allow any amount of preceding whitespace
parseUnit :: Parser Block
parseUnit = whiteSpace >> (Block <$> many parseProperty)

parseBlock :: Parser Block
parseBlock = braces parseUnit

parseBlockLiteral :: Parser Expression
parseBlockLiteral = EBlock <$> parseBlock

-- $utilities
--
--

parseSource :: Parser a -> Parser a
parseSource p = whiteSpace >> p

parseString :: Parser a -> String -> Either ParseError a
parseString p = parse (parseSource p) ""

parseAll :: Parser a -> String -> Either ParseError a
parseAll p = parse (parseSource p <* eof) ""
