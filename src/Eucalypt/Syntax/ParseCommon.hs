{-|
Module      : Eucalypt.Syntax.ParseCommon
Description : Basic parsers for syntax elements of Eucalypt
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Syntax.ParseCommon where

import Data.Void (Void)
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

symbol :: String -> Parser String
symbol = L.symbol sc

colon :: Parser String
colon = symbol ":"

comma :: Parser String
comma = symbol ","

parens :: Parser a -> Parser a
parens = between (symbol "(") (char ')')

braces :: Parser a -> Parser a
braces = between (symbol "{") (char '}')

-- | A normal (non-operator) identifier, n.b. leading digits are legal
-- in Eucalypt: @5v@ is an identifier.
normalIdentifier :: Parser String
normalIdentifier =
  ((:) <$> normalIdentStartChar <*> many normalIdentContinuationChar) <?>
  "normal identifier"
  where
    normalIdentStartChar = letterChar <|> oneOf ("•$?_" :: String)
    normalIdentContinuationChar = alphaNumChar <|> oneOf ("$?!_-*" :: String)
