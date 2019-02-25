{-# LANGUAGE RecordWildCards #-}
module Eucalypt.HarnessSpec
  ( main
  , spec
  ) where

import Data.Maybe (catMaybes)
import Eucalypt.Driver.Evaluator
import Eucalypt.Driver.Options
  ( Command(..)
  , CommandLineMode(..)
  , EucalyptOptions(..)
  , preprocessOptions
  )
import Eucalypt.Syntax.Input
import System.Exit
import Test.Hspec

main :: IO ()
main = hspec spec

removeStdIn :: [Input] -> [Input]
removeStdIn = filter (not . isStdIn) 
  where
    isStdIn Input {inputLocator = StdInput} = True
    isStdIn _ = False

postprocessForTest :: EucalyptOptions -> EucalyptOptions
postprocessForTest opts@EucalyptOptions {..} =
  opts {optionInputs = removeStdIn optionInputs}

run :: FilePath -> IO ExitCode
run f = do
  opts <-
    postprocessForTest <$>
    preprocessOptions
      EucalyptOptions
        { optionMode = Batch
        , optionExportFormat = Nothing
        , optionTarget = Nothing
        , optionOutput = Nothing
        , optionEvaluand = Nothing
        , optionInhibitPrelude = False
        , optionCommand = Headless
        , optionInputs = catMaybes [parseInputFromString f]
        , optionDebug = False
        , optionLibPath = ["harness"]
        }
  evaluate opts

acceptanceSpec :: FilePath -> Spec
acceptanceSpec testFile = describe testFile $
  it "exits with zero exit code" $
  run ("harness/test/" ++ testFile) `shouldReturn` ExitSuccess

spec :: Spec
spec = do
  acceptanceSpec "001_ski.eu"
  acceptanceSpec "002_null.eu"
  acceptanceSpec "003_unicode.eu"
  acceptanceSpec "004_bools.eu"
  acceptanceSpec "005_if.eu"
  acceptanceSpec "006_lists.eu"
  acceptanceSpec "007_recursion.eu"
  acceptanceSpec "008_folds.eu"
  acceptanceSpec "009_lazy.eu"
  acceptanceSpec "010_prelude.eu"
  acceptanceSpec "011_yaml.yaml"
  acceptanceSpec "012_arith.eu"
  acceptanceSpec "013_arith_overflow.eu"
  acceptanceSpec "014_numeric_combinators.eu"
  acceptanceSpec "015_block_fns.eu"
  acceptanceSpec "016_string_fns.eu"
  acceptanceSpec "017_namespacing.eu"
  acceptanceSpec "018_metadata.eu"
  acceptanceSpec "019_env.eu"
  acceptanceSpec "020_op_precedence.eu"
  acceptanceSpec "021_calls_and_lookups.eu"
  acceptanceSpec "022_sections.eu"
  acceptanceSpec "023_yaml_embedding.yaml"
  acceptanceSpec "024_interpolation.eu"
  acceptanceSpec "025_updates.eu"
  acceptanceSpec "026_imports.eu"
  acceptanceSpec "027_unit_import.eu"
  acceptanceSpec "028_toml.toml"
  acceptanceSpec "029_gen_lookup_static.eu"
  acceptanceSpec "030_text_import.eu"
  acceptanceSpec "031_block_anaphora.eu"
  acceptanceSpec "032_number_parse.eu"
  acceptanceSpec "033_scans.eu"
  acceptanceSpec "034_letters.eu"
  acceptanceSpec "036_takes_and_drops.eu"
  acceptanceSpec "038_partitions.eu"
  acceptanceSpec "039_tags.eu"
  acceptanceSpec "040_preserve_tags.yaml"
  acceptanceSpec "041_numeric_formats.eu"
  acceptanceSpec "042_dot_lookup_precedence.eu"
  acceptanceSpec "043_gen_lookup_dynamic.eu"
  acceptanceSpec "044_time.eu"
  acceptanceSpec "045_csv_import.eu"
