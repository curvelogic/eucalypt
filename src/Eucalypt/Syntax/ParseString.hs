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
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.Error
import Eucalypt.Syntax.ParseCommon
import Text.Megaparsec
import Text.Megaparsec.Char


-- | Parse the target of an interpolation request (empty, 0-9 or ref)
target :: Parser Target
target =
  (Reference <$> normalIdentifier) <|>
  (Anaphor <$> optional (digitToInt <$> digitChar))


-- | Parse the format string
format :: Parser String
format = some (satisfy $ \ c -> c /= '}' && c /= ':')


-- | Parse the conversion string
conversion :: Parser String
conversion = some (anySingleBut '}')


-- | Parse an interpolation request
interpolationRequest :: Parser InterpolationRequest
interpolationRequest = braces $ do
  t <- target
  f <- optional (colon >> format)
  c <- optional (colon >> conversion)
  return $ InterpolationRequest t f c


-- | Text with braces escaped. '{{...}}' gives literal content '{...}'
escapedBracesText :: Parser String
escapedBracesText = do
  opens <- some $ char '{'
  let n = length opens
  text <- manyTill anySingle (count n (char '}'))
  return $ replicate (n - 1) '{' ++ text ++ replicate (n - 1) '}'


-- | Parse literal content up to next '{'
literalContent :: Parser String
literalContent = some (satisfy $ \c -> c /= '{' && c /= '"')


-- | Parse a chunk of the string literal content
stringChunk :: Parser StringChunk
stringChunk =
  (LiteralContent <$> literalContent) <|>
  try (lookAhead (string "{{") >> LiteralContent <$> escapedBracesText) <|>
  try (lookAhead (char '{') >> Interpolation <$> interpolationRequest)


-- | Parse content of a string literal (with interpolation syntax)
quotedStringContent :: Parser [StringChunk]
quotedStringContent = many stringChunk

-- | Parse the string content into either error or chunk list
parseEucalyptString :: String -> Either SyntaxError [StringChunk]
parseEucalyptString = first MegaparsecError <$> parse quotedStringContent "<<literal>>"
