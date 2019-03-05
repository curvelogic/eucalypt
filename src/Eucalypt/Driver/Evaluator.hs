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
import Eucalypt.Core.Desugar (translateExpressionToCore)
import Eucalypt.Core.Import
import Eucalypt.Core.Pretty
import Eucalypt.Core.Simplify (simplify)
import Eucalypt.Core.SourceMap
import Eucalypt.Core.Syn
import Eucalypt.Core.Target
import Eucalypt.Core.Unit
import Eucalypt.Core.Verify (runChecks)
import Eucalypt.Driver.IOSource (prepareIOUnit)
import Eucalypt.Driver.Options (Command(..), EucalyptOptions(..), finalise, mergeTargetSettingsIntoOptions)
import qualified Eucalypt.Driver.Stg as STG
import qualified Eucalypt.Driver.Core as Core
import Eucalypt.Reporting.Error (EucalyptError(..))
import Eucalypt.Reporting.Report
  ( tryOrReportUsingSourceMap
  , tryOrReportWithCode
  )
import Eucalypt.Syntax.Ast
import Eucalypt.Syntax.Input
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
      tgtName t ++ format t ++ " [ path: " ++ fmtPath t ++ " ] " ++ "\n\n      " ++ tgtDoc t ++ "\n"
    outputInput i = putStrLn $ "  - " ++ show i
    fmtPath t = intercalate "." $ tgtPath t
    format t = case tgtFormat t of
      Just fmt -> " (as: " ++ fmt ++ ")"
      Nothing -> ""



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
parseEvaluand = flip PE.parseExpression (show CLIEvaluand)



-- | Parse, desugar, and create unit for evaluand.
--
-- (Discards source map so the SMID is essentially useless at present)
readEvaluand :: String -> SMID -> IO CoreExpr
readEvaluand src baseSMID =
  either (throwM . Syntax) return $
  varify . truCore . translateExpressionToCore nullImportHandler baseSMID <$>
  parseEvaluand src



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
formEvaluand :: Maybe String -> Maybe TargetSpec -> TranslationUnit -> IO CoreExpr
formEvaluand evaluand target TranslationUnit {..} =
  case evalSource of
    Nothing -> return truCore
    Just source ->
      rebody truCore <$> readEvaluand source (nextSMID 1 truSourceMap)
  where
    evalSource = evaluand <|> fmap (intercalate "." . tgtPath) target



-- | Implement the Evaluate command, read files and render
evaluate :: EucalyptOptions -> IO ExitCode
evaluate opts = do

  --  Preload specified inputs (not transitive imports) - which helps
  --  make sure we have source available for unexpected errors
  basicLoader <- Core.preloadInputs opts

  -- In the first phase, we translate text into core. Any source
  -- locations are embedded directly in exceptions and reportable
  -- directly.
  (unit, cachingLoader) <- tryOrReportWithCode (Core.loadInput basicLoader) $ do

    when (cmd == Parse) (Core.parseAndDumpASTs opts >> exitSuccess)

    -- Stage 1: parse inputs and translate to core units
    (parsedUnits, ldr) <-
      {-# SCC "ParseToCore" #-} Core.parseInputsAndImports basicLoader $ optionInputs opts

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

    return (merged, ldr)

  -- In the second phase, we optimise then execute and may need a
  -- source map to trace exceptions back to the relevant source code.
  tryOrReportUsingSourceMap (Core.loadInput cachingLoader) (truSourceMap unit) $ do

    -- If a target has been requested, merge any settings associated
    -- with the target into our final options
    let target = do
          tgt <- optionTarget opts <|> Just "main"
          headMay $ filter ((== tgt) . tgtName) (truTargets unit)
    let finalOptions = case target of
          (Just t) -> finalise $ mergeTargetSettingsIntoOptions t opts
          Nothing -> opts

    -- Stage 5: form an expression to evaluate from the source or
    -- command line and embed it in the core tree
    evaluand <- {-# SCC "FormEvaluand" #-} formEvaluand (optionEvaluand finalOptions) target unit
    when (cmd == DumpEvalSubstituted)
      (putStrLn (pprint evaluand) >> exitSuccess)

    -- Stage 6: cook operator soups to resolve all fixities and prepare
    -- a final tree for evaluation
    cookedEvaluand <- {-# SCC "FixityPass" #-} runFixityPass evaluand
    when (cmd == DumpCooked)
      (putStrLn (pprint cookedEvaluand) >> exitSuccess)

    -- Stage 7: simplify core to reduce compile, optimise code and
    -- make debugging STG impelmentation a bit easier
    let finalEvaluand = {-# SCC "Simplification" #-} simplify cookedEvaluand
    when (cmd == DumpFinalCore)
      (putStrLn (pprint finalEvaluand) >> exitSuccess)

    -- Stage 8: run final checks
    let failures = {-# SCC "VerifyCore" #-} runChecks finalEvaluand
    unless (null failures) $ throwM $ Multiple (map Core failures)

    when (cmd == DumpStg)
      (STG.dumpStg finalOptions finalEvaluand >> exitSuccess)

    -- For debug / profile, allow evaluating without any renderer
    -- attached:
    when (cmd == Headless)
      (STG.runHeadless finalOptions finalEvaluand)

    -- Stage 9: drive the evaluation by rendering it
    -- Compile to STG and execute in machine
    when (cmd == Evaluate) $ do
      bytes <- {-# SCC "RenderBytes" #-} STG.renderConduit finalOptions finalEvaluand
      {-# SCC "OutputBytes" #-} outputBytes finalOptions bytes

    return ExitSuccess

  where
    cmd = optionCommand opts


-- | Output the rendered bytes to the specified output
outputBytes :: EucalyptOptions -> BS.ByteString -> IO ()
outputBytes opts s =
  case optionOutput opts of
    Just file -> T.writeFile file (T.decodeUtf8 s)
    Nothing -> T.putStrLn (T.decodeUtf8 s)
