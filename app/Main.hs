module Main
where

import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad.IO.Class
import System.Directory ( doesFileExist, getCurrentDirectory , getHomeDirectory)
import Control.Monad ( liftM, filterM )
import Path

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
  , exportFormat :: String
  , target :: Maybe String
  , output :: Maybe String
  , evaluand :: Maybe String
  , inhibitPrelude :: Bool
  , command :: Command
  , inputs :: [String]
  } deriving (Show)

-- | Alternative ways of specifying output format, -x / -j
exportOption :: Parser String
exportOption = strOption ( long "exportType"
                  <> short 'x'
                  <> metavar "FORMAT"
                  <> value "yaml"
                  <> help "Format for export (e.g. yaml, json)" )
               <|>
               flag' "json"
               ( long "json"
               <> short 'j'
               <> help "JSON output (equivalent to -x json)" )

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
  <*> many (argument str (metavar "INPUTS..."))

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
toAbs dir s = parseRelFile s >>= (return . (dir </>))

-- | If the directory contains Eufile, return it
hasEufile :: Path Abs Dir -> IO (Maybe (Path Abs File))
hasEufile dir = do
  files <- mapM (toAbs dir) ["eufile", "Eufile"]
  existingFiles <- filterM (doesFileExist . toFilePath) files
  return $ case existingFiles of
    [] -> Nothing
    x:_ -> Just x

prependInputs :: EucalyptOptions -> [String] -> EucalyptOptions
prependInputs opts is = opts { inputs = is ++ inputs opts }

-- | Insert Eufile into the inputs lits
insertEufile :: EucalyptOptions -> IO EucalyptOptions
insertEufile opts = do
  dir <- pwd
  eufile <- findInParents hasEufile dir
  return $ case eufile of
    Nothing -> opts
    Just path -> prependInputs opts [toFilePath path]

-- | Add home directory .eucalpyt if it exists
insertHomeFile :: EucalyptOptions -> IO EucalyptOptions
insertHomeFile opts = do
  home <- getHomeDirectory >>= parseAbsDir
  homeFile <- parseRelFile ".eucalypt"
  let abshome = toFilePath (home </> homeFile)
  exists <- doesFileExist abshome
  return $ if exists then prependInputs opts [abshome] else opts

-- | In ergonomic mode, look for .eucalypt, Eufile etc.
processErgonomics :: EucalyptOptions -> IO EucalyptOptions
processErgonomics opts = do
  opts <- insertEufile opts
  opts <- insertHomeFile opts
  return opts

-- |
preprocessOptions :: EucalyptOptions -> IO EucalyptOptions
preprocessOptions opts =
  case mode opts of
    Ergonomic -> processErgonomics opts
    Batch -> return opts

-- | For now, just explain
main :: IO ()
main = do
  options <- (execParser opts) >>= preprocessOptions
  (putStrLn . show) options
  where opts = info (options <**> helper) ( fullDesc
                                            <> progDesc "Run eucalypt transformations"
                                            <> header "eu - command line interface to Eucalypt" )
