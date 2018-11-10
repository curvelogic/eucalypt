{-# LANGUAGE RecordWildCards #-}
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
import Control.Exception.Safe (throwM)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.List (intercalate)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Eucalypt.Core.BlockAnaphora (anaphorise)
import Eucalypt.Core.Cook (cookAllSoup, distributeFixities, runInterpreter)
import Eucalypt.Core.Desugar (translateExpressionToCore, varify)
import Eucalypt.Core.Eliminate (prune, compress)
import Eucalypt.Core.Inliner (inline)
import Eucalypt.Core.Pretty
import Eucalypt.Core.SourceMap
import Eucalypt.Core.Syn
import Eucalypt.Core.Target
import Eucalypt.Core.Unit
import Eucalypt.Core.Verify
import Eucalypt.Driver.IOSource (prepareIOUnit)
import Eucalypt.Driver.Options (Command(..), EucalyptOptions(..))
import qualified Eucalypt.Driver.Stg as STG
import qualified Eucalypt.Driver.Core as Core
import Eucalypt.Reporting.Error (EucalyptError(..))
import Eucalypt.Reporting.Report
  ( tryOrReportUsingSourceMap
  , tryOrReportWithCode
  )
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.Error (SyntaxError(..))
import qualified Eucalypt.Syntax.ParseExpr as PE
import Safe (headMay)
import System.Exit

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



-- | Run a pass to use known fixities to rearrange all operator
-- expression
runFixityPass :: CoreExpr -> IO CoreExpr
runFixityPass expr =
  let distributed = distributeFixities expr
   in case runInterpreter (cookAllSoup distributed) of
        Right result -> return result
        Left err -> throwM $ Core err



-- | Parse text from -e option as expression
parseEvaluand :: String -> Either SyntaxError Expression
parseEvaluand = flip PE.parseExpression "[cli evaluand]"



-- | Parse, desugar, and create unit for evaluand.
--
-- (Discards source map so the SMID is essentially useless at present)
readEvaluand :: String -> SMID -> IO CoreExpr
readEvaluand src baseSMID =
  either (throwM . Syntax) return $
  varify . truCore . translateExpressionToCore baseSMID <$> parseEvaluand src



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
formEvaluand :: EucalyptOptions -> TranslationUnit -> IO CoreExpr
formEvaluand EucalyptOptions {..} TranslationUnit {..} =
  case evalSource of
    Nothing -> return truCore
    Just source ->
      rebody truCore <$> readEvaluand source (nextSMID 1 truSourceMap)
  where
    findTarget tgt =
      headMay $ map (fmtPath . tgtPath) $ filter ((== tgt) . tgtName) truTargets
    evalSource =
      optionEvaluand <|> (optionTarget >>= findTarget) <|> findTarget "main"
    fmtPath = intercalate "."



-- | Implement the Evaluate command, read files and render
evaluate :: EucalyptOptions -> IO ExitCode
evaluate opts = do

  -- In the first phase, we translate text into core. Any source
  -- locations are embedded directly in exceptions and reportable
  -- directly.
  unit <- tryOrReportWithCode Core.readInput $ do

    when (cmd == Parse) (Core.parseAndDumpASTs opts >> exitSuccess)

    -- Stage 1: parse inputs and translate to core units
    parsedUnits <- {-# SCC "ParseToCore" #-} Core.parseInputsAndImports $ optionInputs opts

    -- Stage 2: process any block anaphora
    let units = {-# SCC "BlockAnaphora" #-} map (fmap anaphorise) parsedUnits

    -- Stage 3: prepare an IO unit to contain launch environment data
    io <- prepareIOUnit

    -- Stage 4: merge all units and bind any cross unit refs
    let merged = {-# SCC "MergeUnits" #-} mergeTranslationUnits (io : units)
    let targets = truTargets merged
    let core = truCore merged

    when (cmd == DumpDesugared)
      (putStrLn (pprint core) >> exitSuccess)
    when (cmd == ListTargets)
      (listTargets opts targets >> exitSuccess)

    return merged

  -- In the second phase, we optimise then execute and may need a
  -- source map to trace exceptions back to the relevant source code.
  tryOrReportUsingSourceMap Core.readInput (truSourceMap unit) $ do

    -- Stage 5: form an expression to evaluate from the source or
    -- command line and embed it in the core tree
    evaluand <- {-# SCC "FormEvaluand" #-} formEvaluand opts unit
    when (cmd == DumpEvalSubstituted)
      (putStrLn (pprint evaluand) >> exitSuccess)

    -- Stage 6: cook operator soups to resolve all fixities and prepare
    -- a final tree for evaluation
    cookedEvaluand <- {-# SCC "FixityPass" #-} runFixityPass evaluand
    when (cmd == DumpCooked)
      (putStrLn (pprint cookedEvaluand) >> exitSuccess)

    -- Stage 7: dead code elimination to reduce compile and make
    -- debugging STG impelmentation a bit easier
    let prunedEvaluand = {-# SCC "DeadCodeElimination" #-} prune $ prune $ prune $ prune cookedEvaluand
    when (cmd == DumpPrunedCore)
      (putStrLn (pprint prunedEvaluand) >> exitSuccess)

    -- Now some inlining
    let inlinedEvaluand = {-# SCC "Inlining" #-} prune $ prune $ inline $ inline $ inline prunedEvaluand
    let compressedEvaluand = {-# SCC "Compression" #-} compress inlinedEvaluand

    let finalEvaluand = compressedEvaluand
    when (cmd == DumpFinalCore)
      (putStrLn (pprint finalEvaluand) >> exitSuccess)

    -- Stage 8: run final checks
    let failures = {-# SCC "VerifyCore" #-} runChecks finalEvaluand
    unless (null failures) $ throwM $ Multiple (map Core failures)

    -- Stage 9: drive the evaluation by rendering it
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
