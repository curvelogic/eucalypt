module Eucalypt.Driver.Evaluator
where

import Eucalypt.Driver.Options (EucalyptOptions(..))
import Eucalypt.Driver.Input (Input(..))
import Eucalypt.Syntax.Ast (Expression)
import Eucalypt.Syntax.Parser (parseAll)
import Eucalypt.Core.Syn
import Eucalypt.Core.Desugar (desugarExp)
import Eucalypt.Render.Classes
import Eucalypt.Render (configureRenderer)

-- | Resolve a unit, source and parse the content
readInput :: Input -> IO Expression
readInput = undefined

mergeUnits :: [CoreExp CoreBindingName] -> CoreExp CoreBindingName
mergeUnits = undefined

-- | Implement the Evaluate command, read files and render
evaluate :: EucalyptOptions -> IO ()
evaluate opts = do

  -- Read all inputs
  trees <- mapM readInput (inputs opts)

  -- Desugar and merge ready for evaluation
  let core = mergeUnits . (map desugarExp) $ trees

  let renderer = configureRenderer opts

  -- Eval and render to bytestring
  let bytestring = renderBytes renderer core

  return ()
