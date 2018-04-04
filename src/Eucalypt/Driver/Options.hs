module Eucalypt.Driver.Options
  where

import Eucalypt.Driver.Input ( Input(..), parseInputFromString )
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad.IO.Class
import System.Directory ( doesFileExist, getCurrentDirectory , getHomeDirectory)
import Control.Monad ( liftM, filterM )
import Data.Maybe ( fromJust )
import Path
import System.FilePath ( takeExtension)
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO ( stdInput )

-- | Command line can be used in ergonomic mode for interactive use or
-- batch mode for repeatable builds and batch output
data CommandLineMode = Ergonomic | Batch
  deriving (Show, Eq)

-- | What we're doing: e.g. explain and exit or evaluate and render
data Command = Explain | Evaluate
  deriving (Show, Eq)

-- | Eucalypt command line options
data EucalyptOptions = EucalyptOptions
  { mode :: CommandLineMode
  , exportFormat :: Maybe String
  , target :: Maybe String
  , output :: Maybe String
  , evaluand :: Maybe String
  , inhibitPrelude :: Bool
  , command :: Command
  , inputs :: [Input]
  } deriving (Show)

-- | Alternative ways of specifying output format, -x / -j
exportOption :: Parser (Maybe String)
exportOption = optional $ strOption ( long "exportType"
                                      <> short 'x'
                                      <> metavar "FORMAT"
                                      <> help "Format for export (e.g. yaml, json)" )
               <|>
               flag' "json" ( long "json"
                              <> short 'j'
                              <> help "JSON output (equivalent to -x json)" )

parseInputArgument :: ReadM Input
parseInputArgument = maybeReader $ \s -> parseInputFromString s

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
  <*> flag Evaluate Explain ( long "explain"
                            <> short 'n'
                            <> help "Explain command line interpretation and exit" )
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
  return $ case existingFiles of
    [] -> Nothing
    x:_ -> Just x

-- | Add an input to the start of the list
prependInputs :: EucalyptOptions -> [Input] -> EucalyptOptions
prependInputs opts is = opts { inputs = is ++ inputs opts }

-- | Add an input to the start of the list
appendInputs :: EucalyptOptions -> [Input] -> EucalyptOptions
appendInputs opts is = opts { inputs = inputs opts ++ is }

-- | Insert Eufile into the inputs lits
insertEufile :: EucalyptOptions -> IO EucalyptOptions
insertEufile opts = do
  dir <- pwd
  eufile <- findInParents hasEufile dir
  return $ case eufile of
    Nothing -> opts
    Just path -> prependInputs opts [(fromJust . parseInputFromString . toFilePath) path]

-- | Add home directory .eucalpyt if it exists
insertHomeFile :: EucalyptOptions -> IO EucalyptOptions
insertHomeFile opts = do
  home <- getHomeDirectory >>= parseAbsDir
  homeFile <- parseRelFile ".eucalypt"
  let abshome = toFilePath (home </> homeFile)
  exists <- doesFileExist abshome
  return $ if exists then prependInputs opts [(fromJust . parseInputFromString) abshome] else opts

-- | In ergonomic mode, look for .eucalypt, Eufile etc.
processErgonomics :: EucalyptOptions -> IO EucalyptOptions
processErgonomics opts =
  case mode opts of
    Ergonomic -> insertEufile opts >>= insertHomeFile
    Batch -> return opts

-- | Fill in missing output format, taking output into account
inferOutputFormat :: EucalyptOptions -> IO EucalyptOptions
inferOutputFormat opts = return $
  case exportFormat opts of
    Nothing -> opts { exportFormat = (takeExtension <$> output opts >>= extToFormat) <|> Just "yaml" }
    Just _ -> opts
  where extToFormat ext = case ext of
          ".json" -> Just "json"
          ".yaml" -> Just "yaml"
          ".yml" -> Just "yaml"
          ".eu" -> Just "eu"
          _ -> Just "yaml"


-- | Default-append the standard input to inputs if we're in a pipeline
defaultStdInput :: EucalyptOptions -> IO EucalyptOptions
defaultStdInput opts = do
  istty <- queryTerminal stdInput
  putStrLn ("terminal?" ++ show istty)
  if istty
    then return opts
    else return $ appendInputs opts [ (fromJust . parseInputFromString) "-" ]

-- | Preprocess options in ergonomic mode
preprocessOptions :: EucalyptOptions -> IO EucalyptOptions
preprocessOptions opts =
  inferOutputFormat opts >>= defaultStdInput >>= processErgonomics

getOptions :: IO EucalyptOptions
getOptions =
  execParser opts >>= preprocessOptions
  where opts = info (options <**> helper) ( fullDesc
                                            <> progDesc "Run eucalypt transformations"
                                            <> header "eu - command line interface to Eucalypt" )
