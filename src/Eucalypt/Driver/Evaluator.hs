{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module Eucalypt.Driver.Evaluator
where

import Control.Exception.Safe (try)
import Control.Monad (forM_)
import Data.Bifunctor (second)
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Data.Yaml as Y
import Eucalypt.Core.Desugar (desugar)
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Eucalypt.Driver.Error (CommandError(..))
import Eucalypt.Driver.Input (Input(..), InputMode(..), Locator(..))
import Eucalypt.Driver.Options (Command(..), EucalyptOptions(..))
import Eucalypt.Driver.Lib (getResource)
import Eucalypt.Render (configureRenderer)
import Eucalypt.Render.Classes
import Eucalypt.Reporting.Error (EucalyptError(..))
import Eucalypt.Reporting.Report (reportErrors)
import Eucalypt.Source.Error (DataParseException(..))
import Eucalypt.Source.YamlSource
import Eucalypt.Syntax.Ast (Expression)
import Eucalypt.Syntax.Error (SyntaxError(..))
import Eucalypt.Syntax.Parser (parseNamedInput, parseTopLevel)
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



-- | Merge core units together
mergeUnits :: [CoreExpr] -> CoreExpr
mergeUnits lets = foldl1 CoreApp newLets
  where bindLists = map (\(CoreLet bs _) -> bs) lets
        bodies = map (\(CoreLet _ b) -> b) lets
        bindLists' = scanl1 rebindBindings bindLists
        bodies' = zipWith rebindBody bodies bindLists'
        rebindBindings establishedBindings nextBindings =
          let abstr = abstractNameScope (\n -> (length nextBindings +) <$> (n `elemIndex` map fst establishedBindings)) in
            let reboundNextBindings = map (second abstr) nextBindings in
              reboundNextBindings ++ establishedBindings
        rebindBody oldBody newBindList =
          let abstr = abstractNameScope (`elemIndex` map fst newBindList) in
             abstr oldBody
        newLets = zipWith CoreLet bindLists' bodies'



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



-- | Implement the Evaluate command, read files and render
evaluate :: EucalyptOptions -> WhnfEvaluator -> IO ExitCode
evaluate opts whnfM =
  if optionCommand opts == Parse
    then parseAndDumpASTs opts
    else do
      asts <- mapM parseInput (optionInputs opts)
      case partitionEithers asts of
        ([], []) -> reportErrors [NoSource] >> return (ExitFailure 1)
        ([], units) ->
          render (mergeUnits units) >>= \case
              Left s -> reportErrors [s] >> return (ExitFailure 1)
              Right bytes -> outputBytes opts bytes >> return ExitSuccess
        (errs, _) -> reportErrors errs >> return (ExitFailure 1)
  where
    render = renderBytes (configureRenderer opts) whnfM




-- | Output the rendered bytes to the specified output
outputBytes :: EucalyptOptions -> BS.ByteString -> IO ()
outputBytes opts s =
  case optionOutput opts of
    Just file -> T.writeFile file (T.decodeUtf8 s)
    Nothing -> T.putStrLn (T.decodeUtf8 s)
