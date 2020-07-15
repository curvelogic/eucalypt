{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Syntax.Input
Description : Syntax for specifying inputs, imports, inserts...
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Syntax.Input
  where

import Data.Maybe (fromMaybe)
import Data.Void
import Eucalypt.Syntax.ParseCommon (normalIdentifier)
import Network.URI
import System.FilePath
import Text.Megaparsec



-- | Identifiers the format used to interpret into core
type Format = String



-- | Location of the input source, may be local or remote
data Locator
  = URLInput URI
  | ResourceInput String
  | StdInput
  | CLIEvaluand
  deriving (Eq, Ord)



-- | Resources can be specified as URIs but we
normaliseLocator :: Locator -> Locator
normaliseLocator (URLInput u) | uriScheme u == "resource:" = ResourceInput $ uriPath u
normaliseLocator l = l



-- | Format input location for console or debug
instance Show Locator where
  show l = case l of
    URLInput uri -> show uri
    ResourceInput n -> "[resource:" ++ n ++ "]"
    StdInput -> "[stdin]"
    CLIEvaluand -> "[cli evaluand]"



-- | Description of soure that can be fed to eu
data Input = Input
  { inputLocator :: Locator -- ^ location (file or url)
  , inputName :: Maybe String -- ^ name (for a block to contain the data)
  , inputFormat :: Format -- ^ data format
  } deriving (Eq, Ord)



-- | Format input for console or debug
instance Show Input where
  show i = nameString ++ formatString ++ locatorString
    where locatorString = show (inputLocator i)
          nameString = maybe "" (++ "=") $ inputName i
          formatString = inputFormat i ++ "@"



-- | Use extension to infer format
inferFormat :: Locator -> Maybe Format
inferFormat loc =
  case loc of
    URLInput u -> (extToFormat . takeExtension . uriPath) u
    StdInput -> Just "json"
    ResourceInput _ -> Just "eu"
    CLIEvaluand -> Just "eu"
  where
    extToFormat ext =
      case ext of
        ".json" -> Just "json"
        ".txt" -> Just "text"
        ".toml" -> Just "toml"
        ".yaml" -> Just "yaml"
        ".yml" -> Just "yaml"
        ".csv" -> Just "csv"
        ".eu" -> Just "eu"
        _ -> Nothing



-- | Ensure we've read a valid input
validateLocator :: Locator -> Maybe Locator
validateLocator loc =
  case loc of
    URLInput u ->
      if (isValid . uriPath) u
        then Just loc
        else Nothing
    _ -> Just loc



-- | Read an input locator from string
locatorFromString :: String -> Maybe Locator
locatorFromString s = normaliseLocator <$>
  case s of
    "-" -> Just StdInput
    "[stdin]" -> Just StdInput
    "[cli evaluand]" -> Just CLIEvaluand
    ('[':xs) -> URLInput <$> (parseURI s <|> parseURI (init xs))
    _ -> URLInput <$> (parseURI s <|> parseURI ("file:" ++ s))



normaliseInput :: Input -> Input
normaliseInput i@Input{..} = i { inputLocator = normaliseLocator inputLocator }



-- | Parser for parsing input strings
type Parser = Parsec Void String

-- | Parse an input specifier, e.g. simple.eu, +yaml@data.txt
parseInput :: Parser Input
parseInput = do

  -- Pull the various components apart
  name <- optional $ try (normalIdentifier <* single '=')
  format <- optional $ try (normalIdentifier <* single '@')
  locatorStr <- many anySingle

  -- Try and interpret the URL portion of the input
  let locator = normaliseLocator <$> locatorFromString locatorStr >>= validateLocator

  -- If we have it, infer format
  let extensionFormat = fromMaybe "eu" (locator >>= inferFormat)
  let inferredFormat = fromMaybe extensionFormat format

  case locator of
    Nothing -> fail "Invalid input"
    Just loc -> return Input { inputLocator = loc
                             , inputFormat = inferredFormat
                             , inputName = name }



-- | Parse from string
parseInputFromString :: String -> Maybe Input
parseInputFromString = parseMaybe parseInput
