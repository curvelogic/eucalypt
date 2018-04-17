{-# LANGUAGE OverloadedStrings #-}
module Eucalypt.Driver.Evaluator
where

import Eucalypt.Driver.Options (Command(..), EucalyptOptions(..))
import Eucalypt.Driver.Input (InputMode(..), Input(..), Locator(..))
import Eucalypt.Syntax.Ast (Expression)
import Eucalypt.Syntax.Parser (parseAll, parseTopLevel)
import Eucalypt.Source.YamlSource
import Eucalypt.Core.Syn
import Eucalypt.Core.Desugar (desugarExp)
import Eucalypt.Core.Builtin (euMerge)
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Error
import Eucalypt.Driver.Error (CommandError(..))
import Eucalypt.Render.Classes
import Eucalypt.Render (configureRenderer)
import Eucalypt.Reporting.Error (EucalyptError(..))
import Eucalypt.Reporting.Report (reportErrors)
import Eucalypt.Source.Error (DataParseException(..))
import Eucalypt.Syntax.Error (SyntaxError(..))
import Network.URI
import System.IO
import qualified System.IO.Strict as Strict
import Text.Parsec.Error (ParseError)
import Data.Either (partitionEithers)
import Data.Maybe
import Data.Typeable
import Data.Yaml as Y
import Control.Monad (foldM, forM_)
import Control.Exception.Safe (tryJust, throw, catch, try, Exception)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import System.Exit


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
readInput (ResourceInput n) = error "Resources not implemented"
readInput StdInput = readStdInput



-- | Parse a byteString as eucalypt
parseEucalypt :: BS.ByteString -> Either SyntaxError Expression
parseEucalypt source = parseAll parseTopLevel $ (T.unpack . T.decodeUtf8) source



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
    eucalyptToCore text =
      case parseEucalypt text of
        Left e -> (return . Left . Syntax) e
        Right expr -> (return . Right . desugarExp) expr
    dataToCore text = do
      r <- try (parseYamlData text) :: IO (Either DataParseException CoreExpr)
      case r of
        Left e -> (return . Left . Source) e
        Right core -> (return . Right) core



-- | Merge core units together
mergeUnits :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
mergeUnits whnfM es = case es of
  [] -> Left NoSource
  [x] -> return x
  x:xs -> foldM (euMerge whnfM) x xs -- TODO: builtins in core syn



-- | Dump ASTs
dumpASTs :: EucalyptOptions -> [Expression] -> IO ()
dumpASTs opts exprs = forM_ exprs $ \e ->
  putStrLn "---" >>  (T.putStrLn . T.decodeUtf8 . Y.encode) e



-- | Parse and dump ASTs
parseEucalyptInputs :: EucalyptOptions -> IO ExitCode
parseEucalyptInputs opts = do
  texts <- mapM readInput euLocators
  let (errs, exprs) = partitionEithers (map parseEucalypt texts)
  if null errs
    then dumpASTs opts exprs >> return ExitSuccess
    else reportErrors errs >> return (ExitFailure 1)
  where
    euLocators =
      (map inputLocator . filter (\i -> inputFormat i == "eu") . inputs)
        opts



-- | Implement the Evaluate command, read files and render
evaluate :: EucalyptOptions -> WhnfEvaluator -> IO ExitCode
evaluate opts whnfM =
  if command opts == Parse
    then parseEucalyptInputs opts
    else do
      let renderer = configureRenderer opts
      trees <- mapM parseInput (inputs opts)
      let (lefts, rights) = partitionEithers trees
      if (not . null) lefts
        then reportErrors lefts >> return (ExitFailure 1)
        else case mergeUnits whnfM rights of
               Left s -> reportErrors [s] >> return (ExitFailure 1)
               Right core ->
                 case renderBytes renderer whnfM core of
                   Left s -> reportErrors [s] >> return (ExitFailure 1)
                   Right bytes -> outputBytes opts bytes >> return ExitSuccess



-- | Output the rendered bytes to the specified output
outputBytes :: EucalyptOptions -> BS.ByteString -> IO ()
outputBytes opts str =
  case output opts of
    Just file -> T.writeFile file (T.decodeUtf8 str)
    Nothing -> T.putStrLn (T.decodeUtf8 str)
