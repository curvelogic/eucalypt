{-|
Module      : Eucalypt.Render.Common
Description : Common utilities for all renderers
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.Common where

import Data.Text (Text, pack)
import Eucalypt.Core.Builtin
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn


-- | Return text if the expressio is string-like otherwise runtime error
expectText :: CoreExpr -> Interpreter Text
expectText e =
  case e of
    CorePrim (CoreString s) -> return $ pack s
    CorePrim (CoreSymbol s) -> return $ pack s
    _ -> throwEvalError $ LookupKeyNotStringLike (CoreExpShow e)

-- | Get rid of any metadata returning just the expression for the
-- list item or Nothing if the metadata indicated that export should
-- be suppressed.
boilAwayMetadata :: WhnfEvaluator -> CoreExpr -> Interpreter (Maybe CoreExpr)
boilAwayMetadata whnfM (CoreMeta m e) = do
  meta <- whnfM m
  export <- lookupOr whnfM (return (CorePrim (CoreSymbol "copy"))) meta "export"
  if export == CorePrim (CoreSymbol "suppress")
    then return Nothing
    else return $ Just e
boilAwayMetadata _ e = return $ Just e
