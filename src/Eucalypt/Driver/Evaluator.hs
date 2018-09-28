{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Driver.Evaluator
Description : Main command for running eucalypt transformations
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Driver.Evaluator
where

import Control.Applicative ((<|>))
import Control.Exception.Safe (throwM, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.Loops (iterateUntilM)
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Foldable (traverse_, toList)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml as Y
import Eucalypt.Core.Cook (cookAllSoup, distributeFixities, runInterpreter)
import Eucalypt.Core.Desugar
  ( translateExpressionToCore
  , translateToCore
  , varify
  )
import Eucalypt.Core.Eliminate (prune)
import Eucalypt.Core.Error
import Eucalypt.Core.Import
import Eucalypt.Core.Pretty
import Eucalypt.Core.Syn
import Eucalypt.Core.Target
import Eucalypt.Core.Unit
import Eucalypt.Core.Verify
import Eucalypt.Driver.Error (CommandError(..))
import Eucalypt.Driver.IOSource (prepareIOUnit)
import Eucalypt.Driver.Lib (getResource)
import Eucalypt.Driver.Options (Command(..), EucalyptOptions(..))
import qualified Eucalypt.Driver.Stg as STG
import Eucalypt.Reporting.Error (EucalyptError(..))
import Eucalypt.Reporting.Report (reportErrors)
import Eucalypt.Source.Error (DataParseException(..))
import Eucalypt.Source.TomlSource
import Eucalypt.Source.YamlSource
import Eucalypt.Syntax.Ast (Unit, Expression)
import Eucalypt.Syntax.Error (SyntaxError(..))
import Eucalypt.Syntax.Input (Input(..), Locator(..))
import qualified Eucalypt.Syntax.ParseExpr as PE
import Network.URI
import Safe (headMay)
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
readInput :: Locator -> IO BS.ByteString
readInput (URLInput u) = readURLInput u
readInput (ResourceInput n) = return $ fromJust $ getResource n
readInput StdInput = readStdInput



-- | Parse a byteString as eucalypt
parseEucalypt :: BS.ByteString -> String -> Either SyntaxError Unit
parseEucalypt source = PE.parseUnit text
  where text = (T.unpack . T.decodeUtf8) source



-- | Resolve a unit, read source and parse and desugar the content
-- into CoreExpr, converting all error types into EucalyptErrors.
--
-- Named inputs are automatically set to suppress export as it is
-- assumed that they will be referenced by name in subsequent source.
parseInputToCore :: Input -> IO (Either EucalyptError TranslationUnit)
parseInputToCore i@(Input locator name format) = do
  source <- readInput locator
  case format of
    "toml" -> tomlDataToCore source
    "yaml" -> activeYamlToCore source
    "json" -> yamlDataToCore source
    "eu" -> eucalyptToCore source
    _ -> (return . Left . Command . InvalidInput) i
  where
    maybeApplyName = maybe id applyName name
    eucalyptToCore text =
      case parseEucalypt text (show locator) of
        Left e -> (return . Left . Syntax) e
        Right expr -> (return . Right . maybeApplyName . translateToCore) expr
    yamlDataToCore text = do
      r <- try (parseYamlData text) :: IO (Either DataParseException CoreExpr)
      case r of
        Left e -> (return . Left . Source) e
        Right core -> (return . Right . maybeApplyName . dataUnit) core
    tomlDataToCore text = parseTomlData text >>= (return . Right <$> dataUnit)
    activeYamlToCore text = do
      r <- try (parseYamlExpr text) :: IO (Either DataParseException CoreExpr)
      case r of
        Left e -> (return . Left . Source) e
        Right core -> (return . Right . maybeApplyName . dataUnit) core


-- | Dump ASTs
dumpASTs :: EucalyptOptions -> [Unit] -> IO ()
dumpASTs _ exprs = forM_ exprs $ \e ->
  putStrLn "---" >>  (T.putStrLn . T.decodeUtf8 . Y.encode) e



-- | Parse and dump ASTs
parseAndDumpASTs :: EucalyptOptions -> IO ExitCode
parseAndDumpASTs opts = do
  texts <- traverse readInput euLocators
  let filenames = map show euLocators
  let (errs, units) = partitionEithers (zipWith parseEucalypt texts filenames)
  if null errs
    then dumpASTs opts units >> return ExitSuccess
    else reportErrors errs >> return (ExitFailure 1)
  where
    euLocators =
      (map inputLocator . filter (\i -> inputFormat i == "eu") . optionInputs)
        opts



-- | List the available targets and documentation to standard out
listTargets :: EucalyptOptions -> [TargetSpec] -> IO ExitCode
listTargets opts targets = do
  putStrLn "Available targets\n"
  traverse_ outputTarget targets
  putStrLn "\nFrom inputs\n"
  traverse_ outputInput (optionInputs opts)
  return ExitSuccess
  where
    outputTarget t =
      putStrLn $
      "  - " ++
      tgtName t ++ " [ path: " ++ fmtPath t ++ " ]\t" ++ tgtDoc t
    outputInput i = putStrLn $ "  - " ++ show i
    fmtPath t = intercalate "." $ tgtPath t



-- | Parse units, reporting and exiting on error
parseUnits :: (Traversable t, Foldable t) => t Input -> IO [TranslationUnit]
parseUnits inputs = do
  asts <- traverse parseInputToCore inputs
  case partitionEithers (toList asts) of
    (errs@(_:_), _) -> reportErrors errs >> exitFailure
    ([], []) -> reportErrors [NoSource] >> exitFailure
    ([], units) -> return units



-- | Parse all units in the graph of imports.
--
parseAllUnits :: [Input] -> IO (M.Map Input TranslationUnit)
parseAllUnits inputs = do
  unitMap <- readImportsToMap inputs mempty
  iterateUntilM (null . pendingImports) step unitMap
  where
    readImportsToMap ins m = do
      units <- parseUnits ins
      return $ foldl (\m' (k, v) -> M.insert k v m') m $ zip ins units
    step m = readImportsToMap (toList $ pendingImports m) m
    collectImports = foldMap truImports . M.elems
    collectInputs = M.keysSet
    pendingImports m = S.difference (collectImports m) (collectInputs m)



-- | Run a pass to use known fixities to rearrange all operator
-- expression
runFixityPass :: CoreExpr -> IO CoreExpr
runFixityPass expr =
  let distributed = distributeFixities expr
   in case runInterpreter (cookAllSoup distributed) of
        Right result -> return result
        Left err -> reportErrors [err] >> exitFailure



-- | Parse text from -e option as expression
parseEvaluand :: String -> Either SyntaxError Expression
parseEvaluand = flip PE.parseExpression "[cli evaluand]"



-- | Parse, desugar, and create unit for evaluand
readEvaluand :: String -> Either EucalyptError CoreExpr
readEvaluand src =
  first Syntax $
  varify . truCore . translateExpressionToCore <$> parseEvaluand src



-- | Determine what we are evaluating and rendering based on the
-- options passed and the targets declared in metadata.
--
-- - The evaluate cli arg (-e) takes precendence
-- - Then any target (-t) cli arg is searched for
-- - Then any :main metadata is respected
-- - Finally the entire merged soure is evaluated
--
-- If an evaluand is found, it is applied by creating a new unit and
-- merging it into that supplied
formEvaluand :: EucalyptOptions -> [TargetSpec] -> CoreExpr -> IO CoreExpr
formEvaluand opts targets source =
  case evalSource of
    Nothing -> return source
    Just p ->
      case readEvaluand p of
        Left err -> reportErrors [err] >> exitFailure
        Right expr -> return $ rebody source expr
  where
    findTarget tgt =
      headMay $ map (fmtPath . tgtPath) $ filter ((== tgt) . tgtName) targets
    evalSource =
      optionEvaluand opts <|> (optionTarget opts >>= findTarget) <|> findTarget "main"
    fmtPath = intercalate "."



-- | Parse all units specified on command line (or inferred) to core
-- syntax, processing imports on the way to arrive at a map of all
-- units specified directly or indirectly, each with fully realised
-- core expression with values bound by imported lets.
parseInputsAndImports :: [Input] -> IO [TranslationUnit]
parseInputsAndImports inputs = do
  unitMap <- parseAllUnits inputs
  case applyAllImports unitMap of
    Right processedUnitMap ->
      return $ mapMaybe (`M.lookup` processedUnitMap) inputs
    Left cyclicInputs -> throwM $ CyclicInputs cyclicInputs



-- | Implement the Evaluate command, read files and render
evaluate :: EucalyptOptions -> IO ExitCode
evaluate opts = do
  when (cmd == Parse) (parseAndDumpASTs opts >> exitSuccess)

  -- Stage 1: parse inputs and translate to core units
  units <- {-# SCC "ParseToCore" #-} parseInputsAndImports $ optionInputs opts

  -- Stage 2: prepare an IO unit to contain launch environment data
  io <- prepareIOUnit

  -- Stage 3: merge all units and bind any cross unit refs
  let merged = mergeTranslationUnits (io : units)
  let targets = truTargets merged
  let core = truCore merged

  when (cmd == DumpDesugared)
    (putStrLn (pprint core) >> exitSuccess)
  when (cmd == ListTargets)
    (listTargets opts targets >> exitSuccess)

  -- Stage 4: form an expression to evaluate from the source or
  -- command line and embed it in the core tree
  evaluand <- {-# SCC "FormEvaluand" #-} formEvaluand opts targets core
  when (cmd == DumpEvalSubstituted)
    (putStrLn (pprint evaluand) >> exitSuccess)

  -- Stage 5: cook operator soups to resolve all fixities and prepare
  -- a final tree for evaluation
  cookedEvaluand <- {-# SCC "FixityPass" #-} runFixityPass evaluand
  when (cmd == DumpCooked)
    (putStrLn (pprint cookedEvaluand) >> exitSuccess)

  -- Stage 6: dead code elimination to reduce compile and make
  -- debugging STG impelmentation a bit easier
  let finalEvaluand = {-# SCC "DeadCodeElimination" #-} prune $ prune cookedEvaluand
  when (cmd == DumpFinalCore)
    (putStrLn (pprint finalEvaluand) >> exitSuccess)

  -- Stage 7: run final checks
  let failures = {-# SCC "VerifyCore" #-} runChecks finalEvaluand
  unless (null failures) $ reportErrors failures >> exitFailure

  -- Stage 8: drive the evaluation by rendering it
  -- Compile to STG and execute in machine
  when (cmd == DumpStg)
    (STG.dumpStg opts finalEvaluand >> exitSuccess)
  bytes <- {-# SCC "RenderBytes" #-} STG.renderConduit opts finalEvaluand
  {-# SCC "OutputBytes" #-} outputBytes opts bytes >> exitSuccess

  where
    cmd = optionCommand opts



-- | Output the rendered bytes to the specified output
outputBytes :: EucalyptOptions -> BS.ByteString -> IO ()
outputBytes opts s =
  case optionOutput opts of
    Just file -> T.writeFile file (T.decodeUtf8 s)
    Nothing -> T.putStrLn (T.decodeUtf8 s)
