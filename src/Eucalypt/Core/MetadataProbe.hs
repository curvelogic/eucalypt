{-# LANGUAGE LambdaCase #-}
{-|
Module      : Eucalypt.Core.MetadataProbe
Description : A pre-evaluation pass to probe for target metadata
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.MetadataProbe where

import Control.Monad.Trans.Writer.Lazy (Writer, runWriter, tell)
import Data.List (intercalate)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Traversable (forM, mapM)
import Eucalypt.Core.Builtin
import Eucalypt.Core.Error
import Eucalypt.Core.EvalByName
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn

-- | Walk the statically defined root of the core syntax tree,
-- recording declaration metadata against the paths. This expands out
-- all lets at this level (that were inserted by the desugar pass!)
metapass ::
     CoreExpr -> [CoreBindingName] -> Writer [(String, CoreExpr)] CoreExpr
metapass l@CoreLet {} stack = metapass (instantiateLet l) stack
metapass (CoreBlock (CoreList items)) stack =
  CoreBlock . CoreList <$>
  forM
    items
    (\item ->
       case key item of
         (Just k, Nothing) -> metapass item (k : stack)
         (Just k, Just m) -> tell [(path k, m)] >> metapass item (k : stack)
         _ -> return item)
  where
    key (CoreList [CorePrim (CoreSymbol k), _]) = (Just k, Nothing)
    key (CoreMeta m (CoreList [CorePrim (CoreSymbol k), _])) = (Just k, Just m)
    key _ = (Nothing, Nothing)
    path k = intercalate "." $ reverse (k : stack)
metapass (CoreList l) stack = CoreList <$> mapM (`metapass` stack) l
metapass (CoreMeta m e) stack = CoreMeta m <$> metapass e stack
metapass item _ = return item



-- | Run a single WHNF reduction to expose a combined block beneath
-- the top-level merges
exposeBlock :: CoreExpr -> Interpreter CoreExpr
exposeBlock e = whnfM e >>= \case
  b@CoreBlock{} -> return b
  v -> throwEvalError $ TopLevelNotBlock (CoreExpShow v)



-- | Process accessible declaration metadata and retrieve list of
-- annotated paths with metadata annotations
runMetaPass :: CoreExpr -> Interpreter (CoreExpr, [(String, CoreExpr)])
runMetaPass source = harvestTargets <$> exposeBlock source
  where
    harvestTargets e = runWriter (metapass e [])


type TargetSpecs = [(String, String, String)]

-- | Inspect metadata for :target keys and their :doc strings and
-- return them. Result is list of (target, path, doc)
readTargets :: [(String, CoreExpr)] -> TargetSpecs
readTargets = mapMaybe target
  where
    target (path, b) =
      (\t -> (t, path, docString b)) <$>
      case runInterpreter (lookupMaybe whnfM b "target") of
        Right v -> v >>= symbolName
        Left _ -> Nothing
    docString b = fromMaybe "" $ case runInterpreter (lookupMaybe whnfM b "doc") of
        Right v -> v >>= stringContent
        Left _ -> Nothing
