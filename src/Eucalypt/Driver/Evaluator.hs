{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Eucalypt.Driver.Evaluator
where

import Control.Applicative ((<|>))
import Control.Exception.Safe (try)
import Control.Monad (forM_, when)
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml as Y
import Eucalypt.Core.Desugar (desugar)
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.MetadataProbe
import Eucalypt.Core.Pretty
import Eucalypt.Core.Syn
import Eucalypt.Driver.Error (CommandError(..))
import Eucalypt.Driver.IOSource (prepareIOUnit)
import Eucalypt.Driver.Input (Input(..), InputMode(..), Locator(..))
import Eucalypt.Driver.Lib (getResource)
import Eucalypt.Driver.Options (Command(..), EucalyptOptions(..))
import Eucalypt.Render (configureRenderer)
import Eucalypt.Render.Classes
import Eucalypt.Reporting.Error (EucalyptError(..))
import Eucalypt.Reporting.Report (reportErrors)
import Eucalypt.Source.Error (DataParseException(..))
import Eucalypt.Source.YamlSource
import Eucalypt.Syntax.Ast (Expression)
import Eucalypt.Syntax.Error (SyntaxError(..))
import Eucalypt.Syntax.Parser (parseExpression, parseNamedInput, parseTopLevel)
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
parseEucalypt :: BS.ByteString -> String -> Either SyntaxError Expression
parseEucalypt source name = parseNamedInput parseTopLevel name $ (T.unpack . T.decodeUtf8) source



-- | Resolve a unit, read source and parse and desugar the content
-- into CoreExpr, converting all error types into EucalyptErrors.
parseInput :: Input -> IO (Either EucalyptError CoreExpr)
parseInput i@(Input mode locator name format) = do
  source <- readInput locator
  case (mode, format) of
    (Inert, "yaml") -> dataToCore source
    (Inert, "json") -> dataToCore source
    (Active, "eu") -> eucalyptToCore source
    _ -> (return . Left . Command . InvalidInputMode) i
  where
    applyName core = case name of
      Just n -> letexp [(n, core)] (block [element n (var n)])
      Nothing -> core
    eucalyptToCore text =
      case parseEucalypt text (show locator) of
        Left e -> (return . Left . Syntax) e
        Right expr -> (return . Right . applyName . desugar) expr
    dataToCore text = do
      r <- try (parseYamlData text) :: IO (Either DataParseException CoreExpr)
      case r of
        Left e -> (return . Left . Source) e
        Right core -> (return . Right . applyName) core



-- | Dump ASTs
dumpASTs :: EucalyptOptions -> [Expression] -> IO ()
dumpASTs _ exprs = forM_ exprs $ \e ->
  putStrLn "---" >>  (T.putStrLn . T.decodeUtf8 . Y.encode) e



-- | Parse and dump ASTs
parseAndDumpASTs :: EucalyptOptions -> IO ExitCode
parseAndDumpASTs opts = do
  texts <- mapM readInput euLocators
  let filenames = map show euLocators
  let (errs, exprs) = partitionEithers (zipWith parseEucalypt texts filenames)
  if null errs
    then dumpASTs opts exprs >> return ExitSuccess
    else reportErrors errs >> return (ExitFailure 1)
  where
    euLocators =
      (map inputLocator . filter (\i -> inputFormat i == "eu") . optionInputs)
        opts



-- | List the available targets and documentation to standard out
listTargets :: EucalyptOptions -> [(String, String, String)] -> IO ExitCode
listTargets opts annotations = do
  putStrLn "Available targets\n"
  mapM_ outputTarget annotations
  putStrLn "\nFrom inputs\n"
  mapM_ outputInput (optionInputs opts)
  return ExitSuccess
  where outputTarget (t, p, d) = putStrLn $ "  - " ++ t ++ " [ path: " ++ p ++ " ]\t" ++ d
        outputInput i = putStrLn $ "  - " ++ show i



-- | Parse units, reporting and exiting on error
parseUnits :: EucalyptOptions -> IO [CoreExpr]
parseUnits opts = do
  asts <- mapM parseInput (optionInputs opts)
  case partitionEithers asts of
    (errs@(_:_), _) -> reportErrors errs >> exitFailure
    ([], []) -> reportErrors [NoSource] >> exitFailure
    ([], units) -> return  units



-- | Extract relevant metadata annotations, reporting and exiting on error
runMetadataPass :: CoreExpr -> IO (CoreExpr, [(String, CoreExpr)])
runMetadataPass e = case runInterpreter (runMetaPass e) of
  Right (source, annotations) -> return (source, annotations)
  Left err -> reportErrors [err] >> exitFailure



-- | Parse text from -e option as expression
parseEvaluand :: String -> Either SyntaxError Expression
parseEvaluand = parseNamedInput parseExpression "[cli evaluand]"


-- | Parse, desugar, and create unit for evaluand
readEvaluand :: String -> Either EucalyptError CoreExpr
readEvaluand src =
  first Syntax $ desugar <$> parseEvaluand src



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
formEvaluand :: EucalyptOptions -> TargetSpecs -> CoreExpr -> IO CoreExpr
formEvaluand opts targets source =
  case evalSource of
    Nothing -> return source
    Just src ->
      case readEvaluand src of
        Left err -> reportErrors [err] >> exitFailure
        Right expr -> return $ abstractStaticBlock source expr
  where
    findTarget tgt =
      headMay $
      mapMaybe
        (\(t, p, _) ->
           if t == tgt
             then Just p
             else Nothing)
        targets
    evalSource =
      optionEvaluand opts <|> (optionTarget opts >>= findTarget) <|> findTarget "main"



-- | Implement the Evaluate command, read files and render
evaluate :: EucalyptOptions -> WhnfEvaluator -> IO ExitCode
evaluate opts whnfM = do
  when (cmd == Parse) (parseAndDumpASTs opts >> exitSuccess)
  units <- parseUnits opts
  io <- prepareIOUnit
  let merged = mergeUnits (io : units)
  when (cmd == DumpDesugared) (putStrLn (pprint merged) >> exitSuccess)
  (source, annotations) <- runMetadataPass merged
  let targets = readTargets annotations
  when (cmd == ListTargets) (listTargets opts targets >> exitSuccess)
  when (cmd == DumpMetadataProbed) (putStrLn (pprint source) >> exitSuccess)
  evaluand <- formEvaluand opts targets source
  render evaluand >>= \case
    Left s -> reportErrors [s] >> return (ExitFailure 1)
    Right bytes -> outputBytes opts bytes >> return ExitSuccess
  where
    render = renderBytes (configureRenderer opts) whnfM
    cmd = optionCommand opts



-- | Output the rendered bytes to the specified output
outputBytes :: EucalyptOptions -> BS.ByteString -> IO ()
outputBytes opts s =
  case optionOutput opts of
    Just file -> T.writeFile file (T.decodeUtf8 s)
    Nothing -> T.putStrLn (T.decodeUtf8 s)
