{-# LANGUAGE OverloadedStrings #-}
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
  , readInput
  ) where

import Control.Exception.Safe (throwM, try)
import Control.Monad (forM_)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State.Strict
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml as Y
import Eucalypt.Core.Desugar (translateToCore, CoreLoad)
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
import Eucalypt.Source.TextSource
import Eucalypt.Source.TomlSource
import Eucalypt.Source.YamlSource
import Eucalypt.Syntax.Ast (Unit)
import Eucalypt.Syntax.Error (SyntaxError(..))
import Eucalypt.Syntax.Input (Input(..), Locator(..))
import qualified Eucalypt.Syntax.ParseExpr as PE
import Network.URI
import System.Exit
import System.IO



-- $source
--
-- For now source is slurped in, eagerly, in its entirety into a
-- String, whether from stdin, file or URL. We can moved to a conduit
-- streaming model later if it's worth it.



-- | Read from Standard In
readStdInput :: IO BS.ByteString
readStdInput = BS.hGetContents stdin



-- | Read from FileSystem
readFileInput :: FilePath -> IO BS.ByteString
readFileInput = BS.readFile



-- | Delegate to appropriate function to read input
readURLInput :: URI -> IO BS.ByteString
readURLInput u =
  case uriScheme u of
    "file:" -> readFileInput (uriPath u)
    _ -> print ("scheme: " ++ uriScheme u) >> return ""



-- | Read any locator into a bytestring
readInputLocator :: Locator -> IO BS.ByteString
readInputLocator (URLInput u) = readURLInput u
readInputLocator (ResourceInput n) = return $ fromJust $ getResource n
readInputLocator StdInput = readStdInput
readInputLocator CLIEvaluand = error "CLIEvaluand input should not be read by readInputLocator"



-- | Read bytestring content for an Input
readInput :: Input -> IO BS.ByteString
readInput = readInputLocator . inputLocator



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
parseAndDumpASTs opts = do
  texts <- traverse readInputLocator euLocators
  let filenames = map show euLocators
  let (errs, units) = partitionEithers (zipWith parseEucalypt texts filenames)
  if null errs
    then dumpASTs opts units >> return ExitSuccess
    else throwM $ Multiple (map Syntax errs)
  where
    euLocators =
      (map inputLocator . filter (\i -> inputFormat i == "eu") . optionInputs)
        opts



-- | Resolve a unit, read source and parse and desugar the content
-- into CoreExpr, converting all error types into EucalyptErrors.
--
-- Named inputs are automatically set to suppress export as it is
-- assumed that they will be referenced by name in subsequent source.
loadUnit :: Input -> CoreLoad (Either EucalyptError TranslationUnit)
loadUnit i@(Input locator name format) = do
  firstSMID <- get
  source <- liftIO $ readInput i
  coreUnit <-
    liftIO $
    case format of
      "text" -> textDataToCore i source
      "toml" -> tomlDataToCore i source
      "yaml" -> activeYamlToCore i source
      "json" -> yamlDataToCore i source
      "eu" -> eucalyptToCore i firstSMID source
      _ -> (return . Left . Command . InvalidInput) i
  case coreUnit of
    Right u -> put $ (nextSMID firstSMID . truSourceMap) u
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
      r <- try (parseYamlData text) :: IO (Either DataParseException CoreExpr)
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
      r <- try (parseYamlExpr text) :: IO (Either DataParseException CoreExpr)
      case r of
        Left e -> (return . Left . Source) e
        Right core -> (return . Right . maybeApplyName . dataUnit input) core



-- | Parse units, reporting and exiting on error
loadUnits :: (Traversable t, Foldable t) => t Input -> CoreLoad [TranslationUnit]
loadUnits inputs = do
  asts <- traverse loadUnit inputs
  case partitionEithers (toList asts) of
    -- TODO: propagate all errors
    (e:_, _) -> throwM e
    ([], []) -> throwM NoSource
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
parseInputsAndImports :: [Input] -> IO [TranslationUnit]
parseInputsAndImports inputs = do
  (unitMap, _) <- runStateT (loadAllUnits inputs) 1
  case applyAllImports unitMap of
    Right processedUnitMap ->
      return $ mapMaybe (`M.lookup` processedUnitMap) inputs
    Left cyclicInputs -> throwM $ CyclicInputs cyclicInputs
