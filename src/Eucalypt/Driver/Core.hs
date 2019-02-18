{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Eucalypt.Driver.Core
Description : Facilities for loading core from inputs
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Driver.Core
  ( parseInputsAndImports
  , parseAndDumpASTs
  , loadInput
  , loader
  , preloadInputs
  , CoreLoader(..)
  ) where

import Control.Exception.Safe (IOException, catchJust, throwM, try, tryIO)
import Control.Monad (forM_)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (partitionEithers, rights)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml as Y
import Eucalypt.Core.Desugar (translateToCore)
import Eucalypt.Core.Error
import Eucalypt.Core.Import
import Eucalypt.Core.SourceMap
import Eucalypt.Core.Syn
import Eucalypt.Core.Unit
import Eucalypt.Driver.Error (CommandError(..))
import Eucalypt.Driver.Lib (getResource)
import Eucalypt.Driver.Options (EucalyptOptions(..))
import Eucalypt.Reporting.Error (EucalyptError(..))
import Eucalypt.Source.Error (DataParseException(..))
import Eucalypt.Source.CsvSource
import Eucalypt.Source.TextSource
import Eucalypt.Source.TomlSource
import Eucalypt.Source.YamlSource
import Eucalypt.Syntax.Ast (Unit)
import Eucalypt.Syntax.Error (SyntaxError(..))
import Eucalypt.Syntax.Input (Input(..), Locator(..), normaliseLocator)
import qualified Eucalypt.Syntax.ParseExpr as PE
import Network.URI
import Path
import Safe (headMay)
import System.Exit
import System.IO
import System.Posix.Directory



-- | Options relating to the loading of core from various inputs
data CoreLoaderOptions = CoreLoaderOptions
  { loadPath :: [FilePath] -- ^ search directories for relative paths
  , loadEvaluand :: Maybe String -- ^ maybe a command line evaluand
  }



-- | A loader with options and cache
data CoreLoader = CoreLoader
  { clOptions :: CoreLoaderOptions
  , clCache :: M.Map Locator BS.ByteString
  , clNextSMID :: SMID
  }



-- | Create a new core loader from command line options, this tracks
-- state during the load of all inputs
loader :: EucalyptOptions -> CoreLoader
loader EucalyptOptions {..} =
  CoreLoader
    { clOptions =
        CoreLoaderOptions
          {loadPath = optionLibPath, loadEvaluand = optionEvaluand}
    , clCache = mempty
    , clNextSMID = 1
    }


preloadInputs :: EucalyptOptions -> IO CoreLoader
preloadInputs opts@EucalyptOptions {..} =
  execStateT (traverse readInput optionInputs) $ loader opts



-- | Get the next source map identifier to stamp on syntax item
getNextSMID :: CoreLoad SMID
getNextSMID = gets clNextSMID



-- | Update the next source map identifier
setNextSMID :: SMID -> CoreLoad ()
setNextSMID smid = modify (\s -> s { clNextSMID = smid })


-- | Load bytestring for input, using cache first but falling back to
-- retrieving directly otherwise
loadInput :: CoreLoader -> Input -> IO BS.ByteString
loadInput coreLoader input = evalStateT (readInput input) coreLoader



-- | A state wrapping monad for tracking SMID between different
-- translations - we need to ensure unique souce map IDs in all trees
type CoreLoad a = StateT CoreLoader IO a




toAbsDir :: MonadIO m => FilePath -> m (Path Abs Dir)
toAbsDir d = liftIO $
  catchJust
    pathExceptions
    (parseAbsDir d)
    (\_ -> do
       pwd <- getWorkingDirectory >>= parseAbsDir
       dir <- parseRelDir d
       return $ pwd </> dir)



absolutise :: MonadIO m => Path Abs Dir -> FilePath -> m (Path Abs File)
absolutise d f = liftIO $
  catchJust
    pathExceptions
    ((d </>) <$> parseRelFile f)
    (\_ -> parseAbsFile f)



pathExceptions :: PathException -> Maybe PathException
pathExceptions = Just



-- | Read the content for a specified filename, using the configured
-- load path.
readFileFromLoadPath :: FilePath -> CoreLoad BS.ByteString
readFileFromLoadPath file = do
  CoreLoaderOptions {..} <- gets clOptions
  libPathEntries <- traverse toAbsDir loadPath
  filePaths <- traverse (`absolutise` file) libPathEntries
  contents <- rights <$> traverse tryRead filePaths
  case headMay contents of
    Just bs -> return bs
    Nothing -> throwM $ Command $ CouldNotLoadFile file loadPath
  where
    tryRead :: Path b t -> CoreLoad (Either IOException BS.ByteString)
    tryRead p = liftIO $ tryIO $ BS.readFile (toFilePath p)



-- | Read bytestring content from cache if possible else retrieve
-- directly, adding to cache
readInput :: Input -> CoreLoad BS.ByteString
readInput Input {..} = do
  let loc = normaliseLocator inputLocator
  cache <- gets clCache
  case cache M.!? loc of
    Just bs -> return bs
    Nothing -> do
      bs <- readContent loc
      _ <-
        state $ \s@CoreLoader {..} ->
          (bs, s {clCache = M.insert loc bs clCache})
      return bs
  where
    readContent :: Locator -> CoreLoad BS.ByteString
    readContent (URLInput u) = readURLInput u
    readContent (ResourceInput nm) =
      case getResource nm of
        Just content -> return content
        Nothing -> throwM $ Command $ UnknownResource nm
    readContent StdInput = liftIO $ BS.hGetContents stdin
    readContent CLIEvaluand =
      gets (loadEvaluand . clOptions) >>= \case
        Just text -> (return . T.encodeUtf8 . T.pack) text
        Nothing -> throwM $ Command MissingEvaluand
    readURLInput u =
      case uriScheme u of
        "file:" -> readFileFromLoadPath (uriPath u)
        _ -> throwM $ Command $ UnsupportedURLScheme $ uriScheme u



-- | Parse a byteString as eucalypt
parseEucalypt :: BS.ByteString -> String -> Either SyntaxError Unit
parseEucalypt source = PE.parseUnit text
  where text = (T.unpack . T.decodeUtf8) source



-- | Dump ASTs
dumpASTs :: EucalyptOptions -> [Unit] -> IO ()
dumpASTs _ exprs = forM_ exprs $ \e ->
  putStrLn "---" >>  (T.putStrLn . T.decodeUtf8 . Y.encode) e



-- | Parse and dump ASTs
parseAndDumpASTs :: EucalyptOptions -> IO ExitCode
parseAndDumpASTs opts@EucalyptOptions {..} = do
  texts <- evalStateT (traverse readInput euInputs) (loader opts)
  let filenames = map show euInputs
  let (errs, units) = partitionEithers (zipWith parseEucalypt texts filenames)
  if null errs
    then dumpASTs opts units >> return ExitSuccess
    else throwM $ Multiple (map Syntax errs)
  where
    euInputs = filter (\i -> inputFormat i == "eu") optionInputs



-- | Resolve a unit, read source and parse and desugar the content
-- into CoreExpr, converting all error types into EucalyptErrors.
--
-- Named inputs are automatically set to suppress export as it is
-- assumed that they will be referenced by name in subsequent source.
loadUnit :: Input -> CoreLoad (Either EucalyptError TranslationUnit)
loadUnit i@(Input locator name format) = do
  firstSMID <- getNextSMID
  source <- readInput i
  coreUnit <-
    liftIO $
    case format of
      "text" -> textDataToCore i source
      "toml" -> tomlDataToCore i source
      "yaml" -> activeYamlToCore i source
      "json" -> yamlDataToCore i source
      "csv" -> csvDataToCore i source
      "eu" -> eucalyptToCore i firstSMID source
      _ -> (return . Left . Command . InvalidInput) i
  case coreUnit of
    Right u -> setNextSMID $ (nextSMID firstSMID . truSourceMap) u
    Left _ -> return ()
  return coreUnit
  where
    maybeApplyName = maybe id applyName name
    eucalyptToCore input smid text =
      case parseEucalypt text (show locator) of
        Left e -> (return . Left . Syntax) e
        Right expr ->
          (return . Right . maybeApplyName . translateToCore input smid) expr
    yamlDataToCore input text = do
      r <-
        try (parseYamlExpr (show locator) text) :: IO (Either DataParseException CoreExpr)
      case r of
        Left e -> (return . Left . Source) e
        Right core -> (return . Right . maybeApplyName . dataUnit input) core
    textDataToCore input text =
      parseTextLines text >>=
      (return . Right . maybeApplyName <$> dataUnit input)
    tomlDataToCore input text =
      parseTomlData text >>=
      (return . Right . maybeApplyName <$> dataUnit input)
    activeYamlToCore input text = do
      r <-
        try (parseYamlExpr (show locator) text) :: IO (Either DataParseException CoreExpr)
      case r of
        Left e -> (return . Left . Source) e
        Right core -> (return . Right . maybeApplyName . dataUnit input) core
    csvDataToCore input text =
      parseCsv (BL.fromStrict text) >>=
      (return . Right . maybeApplyName <$> dataUnit input)


-- | Parse units, reporting and exiting on error
loadUnits :: Traversable t => t Input -> CoreLoad [TranslationUnit]
loadUnits inputs = do
  asts <- traverse loadUnit inputs
  case partitionEithers (toList asts) of
    ([e], _) -> throwM e
    (es@(_:_), _) -> throwM $ Multiple es
    ([], []) -> throwM $ Core NoSource
    ([], units) -> return units



-- | Parse all units in the graph of imports.
--
loadAllUnits :: [Input] -> CoreLoad (M.Map Input TranslationUnit)
loadAllUnits inputs = do
  unitMap <- readImportsToMap inputs mempty
  iterateUntilM (null . pendingImports) step unitMap
  where
    readImportsToMap ins m = do
      units <- loadUnits ins
      return $ foldl (\m' (k, v) -> M.insert k v m') m $ zip ins units
    step m = readImportsToMap (toList $ pendingImports m) m
    collectImports = foldMap truImports . M.elems
    collectInputs = M.keysSet
    pendingImports m = S.difference (collectImports m) (collectInputs m)



-- | Parse all units specified on command line (or inferred) to core
-- syntax, processing imports on the way to arrive at a map of all
-- units specified directly or indirectly, each with fully realised
-- core expression with values bound by imported lets.
--
-- SourceMap ids are unique across all the units, starting at 1
parseInputsAndImports ::
     CoreLoader -> [Input] -> IO ([TranslationUnit], CoreLoader)
parseInputsAndImports coreLoader inputs = do
  (unitMap, populatedLoader) <- runStateT (loadAllUnits inputs) coreLoader
  case applyAllImports unitMap of
    Right processedUnitMap ->
      return (mapMaybe (`M.lookup` processedUnitMap) inputs, populatedLoader)
    Left cyclicInputs -> throwM $ Command $ CyclicInputs cyclicInputs
