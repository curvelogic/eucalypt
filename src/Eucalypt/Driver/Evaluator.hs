module Eucalypt.Driver.Evaluator
where

import Eucalypt.Driver.Options (EucalyptOptions(..), Command(..))
import Eucalypt.Driver.Input (Input(..), Locator(..))
import Eucalypt.Syntax.Ast (Expression)
import Eucalypt.Syntax.Parser (parseAll, parseTopLevel)
import Eucalypt.Core.Syn
import Eucalypt.Core.Desugar (desugarExp)
import Eucalypt.Core.Builtin (RuntimeError(..),euMerge,runtimeError)
import Eucalypt.Render.Classes
import Eucalypt.Render (configureRenderer)
import Network.URI
import System.IO
import qualified System.IO.Strict as Strict
import Text.Parsec.Error (ParseError)
import Data.Either (partitionEithers)
import Data.Maybe
import Data.Yaml as Y
import Control.Monad (foldM, forM_)
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T

-- | Read from Standard In
readStdInput :: IO String
readStdInput = Strict.hGetContents stdin

-- | Read from FileSystem
readFileInput :: FilePath -> IO String
readFileInput path = withFile path ReadMode Strict.hGetContents

-- | Delegate to appropriate function to read input
readURLInput :: URI -> IO String
readURLInput u =
  case uriScheme u of
    "file:" -> readFileInput (uriPath u)
    _ -> print ("scheme: " ++ (uriScheme u)) >> return ""

-- | Resolve a unit, source and parse the content
readInput :: Input -> IO (Either ParseError Expression)
readInput i = do
  text <- source
  return $ parseAll parseTopLevel text -- TODO: eof & stdin?
  where source = case inputLocator i of
          URLInput u -> readURLInput u
          ResourceInput n -> error("Resources not implemented")
          StdInput -> readStdInput


-- | Merge core units together
mergeUnits :: [CoreExp CoreBindingName] -> Either RuntimeError (CoreExp CoreBindingName)
mergeUnits es = case es of
  [] -> runtimeError "No units to merge"
  x:[] -> Right x
  x:xs -> foldM euMerge x xs


-- | Dump ASTs
dumpASTs :: EucalyptOptions -> [Expression] -> IO ()
dumpASTs opts exprs = forM_ exprs $ \e ->
  putStrLn "---" >>  (T.putStrLn . T.decodeUtf8 . Y.encode) e


-- | Implement the Evaluate command, read files and render
evaluate :: EucalyptOptions -> IO ()
evaluate opts = do

  -- Prepare renderer
  let renderer = configureRenderer opts

  -- Read all inputs
  trees <- mapM readInput (inputs opts)
  let (lefts, rights) = partitionEithers trees
  if (not . null) lefts
    then

    -- Report any parse errors
    reportErrors lefts

    else

      if command opts == Parse then
        -- Dump ASTs
        dumpASTs opts rights

      else

        -- Desugar and merge
        case (mergeUnits . (map desugarExp)) rights of
          Left s ->
            reportErrors [s]

          Right core ->
            case (renderBytes renderer core) of
              Left s ->
                reportErrors [s]

              Right bytes ->
                outputBytes opts bytes

-- | Output the rendered bytes to the specified output
outputBytes :: EucalyptOptions -> BS.ByteString -> IO ()
outputBytes opts str = case output opts of
  Just file -> T.writeFile file (T.decodeUtf8 str)
  Nothing -> T.putStrLn (T.decodeUtf8 str)

-- | Report any errors to stderr
reportErrors :: Show a => [a] -> IO ()
reportErrors = mapM_ (hPrint stderr)
