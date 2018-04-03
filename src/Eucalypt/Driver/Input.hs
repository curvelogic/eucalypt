module Eucalypt.Driver.Input
  where

import Debug.Trace ( trace )
import Text.Parsec ( try )
import Text.Parsec.Prim ( many )
import Text.Parsec.Char ( char, anyChar )
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator ( optionMaybe )
import System.FilePath
import Eucalypt.Syntax.Parser
import Network.URI
import Path
import Data.Maybe ( fromMaybe, fromJust )
import Control.Applicative ( (<|>) )

-- | All inputs are active (evaluated) or inert (data-only), Eucalypt
-- format is always active; yaml and json and be inert or active.
data InputMode = Active | Inert
  deriving (Eq, Show)

-- | Identifiers the format used to interpret into core
type Format = String

-- | Location of the input source, may be local or remote
data Locator = URLInput URI | ResourceInput String | StdInput
  deriving (Eq, Show)

data Input = Input { inputMode :: InputMode        -- ^ active or inert
                   , inputLocator :: Locator       -- ^ location (file or url)
                   , inputName :: Maybe String     -- ^ name (for a block to contain the data)
                   , inputFormat :: Format         -- ^ data format
                   }
  deriving (Eq, Show)

-- | Use extension to infer format
inferFormat :: Locator -> Maybe Format
inferFormat loc = case loc of
  URLInput u -> (extToFormat . takeExtension . uriPath) u
  StdInput -> Nothing
  ResourceInput name -> Just "eu"
  where extToFormat ext = case ext of
          ".json" -> Just "json"
          ".yaml" -> Just "yaml"
          ".yml" -> Just "yaml"
          ".eu" -> Just "eu"
          _ -> Nothing

-- | Ensure we've read a valid input
validateLocator :: Locator -> Maybe Locator
validateLocator loc = case loc of
  URLInput u -> if (isValid . uriPath) u then Just loc else Nothing
  StdInput -> Just loc
  ResourceInput name -> Nothing

locatorFromString :: String -> Maybe Locator
locatorFromString s =
  case s of
    "-" -> Just StdInput
    _ -> URLInput <$> (parseURI s <|> (parseRelativeReference s))

-- | Parse an input specifier, e.g. simple.eu, +yaml@data.txt
parseInput :: Parser Input
parseInput = do

  -- Pull the various components apart
  maybePlus <- optionMaybe $ try (char '+')
  name <- optionMaybe $ try (identifier <* char '=')
  format <- optionMaybe $ try (identifier <* char '@')
  locatorStr <- many anyChar

  -- Try and interpret the URL portion of the input
  let locator = locatorFromString locatorStr >>= validateLocator

  -- If we have it, infer format
  let extensionFormat = fromMaybe "eu" (locator >>= inferFormat)
  let inferredFormat = fromMaybe extensionFormat format

  -- ...and mode
  let defaultMode = if inferredFormat == "eu" then Active else Inert
  let inferredMode = maybe defaultMode (const Active) maybePlus

  case locator of
    Nothing -> fail "Invalid input"
    Just loc -> return $ Input { inputMode = inferredMode
                               , inputLocator =  loc
                               , inputFormat = inferredFormat
                               , inputName = name }

-- | Parse from string
parseInputFromString :: String -> Maybe Input
parseInputFromString s = either (const Nothing) Just (parseString parseInput s)
