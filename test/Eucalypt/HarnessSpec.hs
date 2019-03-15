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
import System.IO (stderr, stdout)
import System.IO.Silently 
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
        , optionCommand = Evaluate
        , optionInputs = catMaybes [parseInputFromString f]
        , optionDebug = False
        , optionLibPath = ["harness"]
        }
  hSilence [stdout, stderr] $ evaluate opts

acceptanceSpec :: FilePath -> Spec
acceptanceSpec testFile = describe testFile $
  it "exits with zero exit code" $
  run ("harness/test/" ++ testFile) `shouldReturn` ExitSuccess

failureSpec :: FilePath -> Spec
failureSpec testFile = describe testFile $
  it "throws exception" $
  run ("harness/test/errors/" ++ testFile) `shouldThrow` anyException

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
  acceptanceSpec "046_commas.eu"
  failureSpec "001_dot_in_metadata_key.eu"
  failureSpec "002_lists.eu"
  failureSpec "003_free_var_arg.eu"
  failureSpec "004_circular.eu"
  failureSpec "005_free_var.eu"
  failureSpec "006_div_by_zero.eu"
  failureSpec "008_op_spacing.eu"
  failureSpec "009_panic.eu"
  failureSpec "010_assert.eu"
  failureSpec "011_assert_pred.eu"
  failureSpec "012_bad_value.eu"
  failureSpec "013_bad_nested_value.eu"
  failureSpec "014_unterm_strlit.eu"
  failureSpec "015_missing_argtuple_close.eu"
  failureSpec "016_empty_brackets.eu"
  failureSpec "017_too_many_args.eu"
  failureSpec "019_no_such_key.eu"
  failureSpec "020_no_such_key_fn.eu"
  failureSpec "021_bad_num_parse.eu"
  failureSpec "023_arg_types.eu"
