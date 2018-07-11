{-|
Module      : Eucalypt.Syntax.ParseString
Description : Parser for the eucalypt string interpolation DSL
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

import Eucalypt.Syntax.ParseCommon
import Text.Megaparsec
import Text.Megaparsec.Char

-- | The target of an interpolation request, either an implicit
-- parameter ("anaphor"), numbered or otherwise or a name referring to
-- value in appropriate block.
data Target
  = Anaphor (Maybe Int) -- empty anaphor or indexed anaphor
  | Reference String -- reference to value from block (or environment)

-- | The interpolation request
--
-- A target and string governing how to format it
data InterpolationRequest = InterpolationRequest
  { refTarget :: Target
  , refFormat :: (Maybe String)
  }

-- | A string element is either literal content or an interpolation
-- request
data StringElement
  = LiteralContent String
  | Interpolation InterpolationRequest


-- | Parse the target of an interpolation request (empty, 0-9 or ref)
parseTarget :: Parser (Maybe Target)
parseTarget =
  (Reference <$> normalIdentifier) <|> (Anaphor <$> optional digitChar)

-- | Parse the format string
parseFormat :: Parser (Maybe String)
parseFormat = optional $ some (notChar '}')

-- | Parse an interpolation request
parseInterpolationRequest :: Parser InterpolationRequest
parseInterpolationRequest = do
  char '{' << notFollowedBy (char '{')
  target <- parseTarget
  format <- optional (colon >> parseFormat)
  char '}'
  return $ InterpolationRequest target format

parseLiteralContent :: Parser String
parseLiteralContent = do
                         text <- anyTill (char '{')


parseStringContent :: Parser [StringElement]
parseStringContent = undefined
