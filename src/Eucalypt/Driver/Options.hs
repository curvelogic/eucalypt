module Eucalypt.Driver.Options
  where

import Control.Monad (filterM)
import Control.Monad.IO.Class
import Data.Maybe (fromJust)
import Data.Semigroup ((<>))
import Eucalypt.Driver.Input (Input(..), parseInputFromString)
import Options.Applicative
import Path
import System.Directory (doesFileExist, getCurrentDirectory, getHomeDirectory)
import System.FilePath (takeExtension)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (queryTerminal)


-- | Command line can be used in ergonomic mode for interactive use or
-- batch mode for repeatable builds and batch output
data CommandLineMode = Ergonomic | Batch
  deriving (Show, Eq)



-- | What we're doing: e.g. explain and exit or evaluate and render
data Command = Explain | Evaluate | Parse
  deriving (Show, Eq)



-- | Eucalypt command line options
data EucalyptOptions = EucalyptOptions
  { optionMode :: CommandLineMode
  , optionExportFormat :: Maybe String
  , optionTarget :: Maybe String
  , optionOutput :: Maybe String
  , optionEvaluand :: Maybe String
  , optionInhibitPrelude :: Bool
  , optionCommand :: Command
  , optionInputs :: [Input]
  } deriving (Show)



-- | Alternative ways of specifying output format, -x / -j
exportOption :: Parser (Maybe String)
exportOption =
  optional $
  strOption
    (long "exportType" <> short 'x' <> metavar "FORMAT" <>
     help "Format for export (e.g. yaml, json)") <|>
  flag'
    "json"
    (long "json" <> short 'j' <> help "JSON output (equivalent to -x json)")



parseInputArgument :: ReadM Input
parseInputArgument = maybeReader $ \s -> parseInputFromString s



commandOption :: Parser Command
commandOption =
  flag'
    Explain
    (long "explain" <> short 'n' <>
     help "Explain command line interpretation and exit") <|>
  flag
    Evaluate
    Parse
    (long "parse" <> short 'p' <>
     help "Parse program text and output AST - do not evaluate")



-- | Parse the command line options
options :: Parser EucalyptOptions
options = EucalyptOptions
  <$> flag Ergonomic Batch ( long "batch"
                             <> short 'B'
                             <> help "Batch (i.e. non-ergonomic) mode" )
  <*> exportOption
  <*> optional (strOption ( long "target"
                            <> short 't'
                            <> metavar "TARGET"
                            <> help "Target name to export (must be defined in the source)" ))
  <*> optional (strOption ( long "output"
                          <> short 'o'
                          <> metavar "FILENAME"
                          <> help "Output file or directory to export to" ))
  <*> optional (strOption ( long "evaluate"
                          <> short 'e'
                          <> metavar "EXPRESSION"
                          <> help "Expression to evaluate and render" ))
  <*> switch ( long "no-prelude"
             <> short 'Q'
             <> help "Don't include standard prelude" )
  <*> commandOption
  <*> many (argument parseInputArgument (metavar "INPUTS..."))



-- | @findInParents f path@ applies @f@ to @path@ and its 'parent's until
-- it finds a 'Just' or reaches the root directory.
findInParents :: MonadIO m => (Path Abs Dir -> m (Maybe a)) -> Path Abs Dir -> m (Maybe a)
findInParents f path = do
  mres <- f path
  case mres of
    Just res -> return (Just res)
    Nothing -> do
      let next = parent path
      if next == path
        then return Nothing
        else findInParents f next



-- | Current working director as Path
pwd :: IO (Path Abs Dir)
pwd = getCurrentDirectory >>= parseAbsDir



-- | Absolute path from rel path and filename
toAbs:: Path Abs Dir -> String -> IO (Path Abs File)
toAbs dir s = (dir </>) <$> parseRelFile s



-- | If the directory contains Eufile, return it
hasEufile :: Path Abs Dir -> IO (Maybe (Path Abs File))
hasEufile dir = do
  files <- mapM (toAbs dir) ["eufile", "Eufile"]
  existingFiles <- filterM (doesFileExist . toFilePath) files
  return $
    case existingFiles of
      [] -> Nothing
      x:_ -> Just x



-- | Add an input to the start of the list
prependInputs :: EucalyptOptions -> [Input] -> EucalyptOptions
prependInputs opts is = opts { optionInputs = is ++ optionInputs opts }



-- | Add an input to the start of the list
appendInputs :: EucalyptOptions -> [Input] -> EucalyptOptions
appendInputs opts is = opts { optionInputs = optionInputs opts ++ is }



-- | Insert Eufile into the inputs lits
insertEufile :: EucalyptOptions -> IO EucalyptOptions
insertEufile opts = do
  dir <- pwd
  eufile <- findInParents hasEufile dir
  return $
    case eufile of
      Nothing -> opts
      Just path ->
        prependInputs opts [(fromJust . parseInputFromString . toFilePath) path]



-- | Add home directory .eucalpyt if it exists
insertHomeFile :: EucalyptOptions -> IO EucalyptOptions
insertHomeFile opts = do
  home <- getHomeDirectory >>= parseAbsDir
  homeFile <- parseRelFile ".eucalypt"
  let abshome = toFilePath (home </> homeFile)
  exists <- doesFileExist abshome
  return $
    if exists
      then prependInputs opts [(fromJust . parseInputFromString) abshome]
      else opts



-- | In ergonomic mode, look for .eucalypt, Eufile etc.
processErgonomics :: EucalyptOptions -> IO EucalyptOptions
processErgonomics opts =
  case optionMode opts of
    Ergonomic -> insertEufile opts >>= insertHomeFile
    Batch -> return opts



-- | Fill in missing output format, taking output into account
inferOutputFormat :: EucalyptOptions -> IO EucalyptOptions
inferOutputFormat opts =
  return $
  case optionExportFormat opts of
    Nothing ->
      opts
        { optionExportFormat =
            (takeExtension <$> optionOutput opts >>= extToFormat) <|> Just "yaml"
        }
    Just _ -> opts
  where
    extToFormat ext =
      case ext of
        ".json" -> Just "json"
        ".yaml" -> Just "yaml"
        ".yml" -> Just "yaml"
        ".eu" -> Just "eu"
        _ -> Just "yaml"



-- | Default-append the standard input to inputs if we're in a pipeline
defaultStdInput :: EucalyptOptions -> IO EucalyptOptions
defaultStdInput opts = do
  istty <- queryTerminal stdInput
  if istty
    then return opts
    else return $ appendInputs opts [ (fromJust . parseInputFromString) "-" ]



-- | Preprocess options in ergonomic mode
preprocessOptions :: EucalyptOptions -> IO EucalyptOptions
preprocessOptions opts =
  inferOutputFormat opts >>= defaultStdInput >>= processErgonomics



getOptions :: IO EucalyptOptions
getOptions = execParser opts >>= preprocessOptions
  where
    opts =
      info
        (options <**> helper)
        (fullDesc <> progDesc "Run eucalypt transformations" <>
         header "eu - command line interface to Eucalypt")