module Eucalypt.Driver.Input
  where

import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe)
import Data.Void
import Eucalypt.Syntax.ParseExpr (normalIdentifier)
import Network.URI
import System.FilePath
import Text.Megaparsec
import Text.Megaparsec.Char

-- | All inputs are active (evaluated) or inert (data-only), Eucalypt
-- format is always active; yaml and json and be inert or active.
data InputMode
  = Active
  | Inert
  deriving (Eq, Show)



-- | Identifiers the format used to interpret into core
type Format = String



-- | Location of the input source, may be local or remote
data Locator
  = URLInput URI
  | ResourceInput String
  | StdInput
  deriving (Eq)



-- | Format input location for console or debug
instance Show Locator where
  show l = case l of
    URLInput uri -> show uri
    ResourceInput n -> "resource:" ++ n
    StdInput -> "[stdin]"



-- | Description of soure that can be fed to eu
data Input = Input
  { inputMode :: InputMode -- ^ active or inert
  , inputLocator :: Locator -- ^ location (file or url)
  , inputName :: Maybe String -- ^ name (for a block to contain the data)
  , inputFormat :: Format -- ^ data format
  } deriving (Eq)



-- | Format input for console or debug
instance Show Input where
  show i = modeString ++ nameString ++ formatString ++ locatorString
    where modeString = if inputMode i == Active then "+" else ""
          locatorString = show (inputLocator i)
          nameString = maybe "" (++ "=") $ inputName i
          formatString = inputFormat i ++ "@"



-- | Use extension to infer format
inferFormat :: Locator -> Maybe Format
inferFormat loc =
  case loc of
    URLInput u -> (extToFormat . takeExtension . uriPath) u
    StdInput -> Nothing
    ResourceInput _ -> Just "eu"
  where
    extToFormat ext =
      case ext of
        ".json" -> Just "json"
        ".yaml" -> Just "yaml"
        ".yml" -> Just "yaml"
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
    StdInput -> Just loc
    ResourceInput _ -> Nothing



-- | Read an input locator from string
locatorFromString :: String -> Maybe Locator
locatorFromString s =
  case s of
    "-" -> Just StdInput
    _ -> URLInput <$> (parseURI s <|> parseURI ("file:" ++ s))


-- | Parser for parsing input strings
type Parser = Parsec Void String

-- | Parse an input specifier, e.g. simple.eu, +yaml@data.txt
parseInput :: Parser Input
parseInput = do

  -- Pull the various components apart
  maybePlus <- optional $ try (char '+')
  name <- optional $ try (normalIdentifier <* char '=')
  format <- optional $ try (normalIdentifier <* char '@')
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
    Just loc -> return Input { inputMode = inferredMode
                             , inputLocator =  loc
                             , inputFormat = inferredFormat
                             , inputName = name }



-- | Parse from string
parseInputFromString :: String -> Maybe Input
parseInputFromString = parseMaybe parseInput
