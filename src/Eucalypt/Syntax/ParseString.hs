{-|
Module      : Eucalypt.Syntax.ParseString
Description : Parser for the eucalypt string interpolation DSL
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Syntax.ParseString where

import Data.Bifunctor (first)
import Data.Char (digitToInt)
import Eucalypt.Syntax.Error
import Eucalypt.Syntax.ParseCommon
import Text.Megaparsec
import Text.Megaparsec.Char

-- | The target of an interpolation request, either an implicit
-- parameter ("anaphor"), numbered or otherwise or a name referring to
-- value in appropriate block.
data Target
  = Anaphor (Maybe Int) -- empty anaphor or indexed anaphor
  | Reference String -- reference to value from block (or environment)
  deriving (Eq, Show)

-- | The interpolation request
--
-- A target and string governing how to format it
data InterpolationRequest = InterpolationRequest
  { refTarget :: Target
  , refFormat :: Maybe String
  } deriving (Eq, Show)

-- | A string element is either literal content or an interpolation
-- request
data StringElement
  = LiteralContent String
  | Interpolation InterpolationRequest
  deriving (Eq, Show)

-- | Parse the target of an interpolation request (empty, 0-9 or ref)
target :: Parser Target
target =
  (Reference <$> normalIdentifier) <|>
  (Anaphor <$> optional (digitToInt <$> digitChar))

-- | Parse the format string
format :: Parser String
format = some (notChar '}')

-- | Parse an interpolation request
interpolationRequest :: Parser InterpolationRequest
interpolationRequest = braces $ do
  t <- target
  f <- optional (colon >> format)
  return $ InterpolationRequest t f

-- | Text with braces escaped. '{{...}}' gives literal content '{...}'
escapedBracesText :: Parser String
escapedBracesText = do
  opens <- some $ char '{'
  let n = length opens
  text <- manyTill anyChar (count n (char '}'))
  return $ replicate (n - 1) '{' ++ text ++ replicate (n - 1) '}'

-- | Parse literal content up to next '{'
literalContent :: Parser String
literalContent = some $ notChar '{'


-- | Parse a chunk of the string literal content
chunk :: Parser StringElement
chunk =
  (LiteralContent <$> literalContent) <|>
  try (lookAhead (string "{{") >> LiteralContent <$> escapedBracesText) <|>
  try (lookAhead (char '{') >> Interpolation <$> interpolationRequest)



-- | Parse content of a string literal (with interpolation syntax)
stringContent :: Parser [StringElement]
stringContent = many chunk

-- | Parse the string content into either error or chunk list
parseEucalyptString :: String -> Either SyntaxError [StringElement]
parseEucalyptString = first MegaparsecError <$> parse stringContent "<<literal>>"
