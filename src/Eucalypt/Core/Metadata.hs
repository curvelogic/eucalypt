{-# LANGUAGE LambdaCase, TupleSections #-}
{-|
Module      : Eucalypt.Core.Metadata
Description : Working with metadata in core
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Metadata where

import Control.Monad (join)
import Data.Maybe
import Eucalypt.Syntax.Input
import Eucalypt.Core.Syn

-- | Read from unevaluated metadata (expanding out only an outer let
-- to prepare a block for lookup).
readUnevaluatedMetadata :: String -> CoreExp a -> (CoreExp a -> b) -> Maybe b
readUnevaluatedMetadata key expr@(CoreLet _ _) readVal =
  readUnevaluatedMetadata key (instantiateLet expr) readVal
readUnevaluatedMetadata key (CoreBlock (CoreList items)) readVal =
  readVal <$> lookup key buildSearchList
  where
    buildSearchList = mapMaybe kv items
    kv (CoreList [CorePrim (CoreSymbol k), v]) = Just (k, v)
    kv (CoreMeta _ i) = kv i
    kv _ = Nothing
readUnevaluatedMetadata _ _ _ = Nothing

-- | Remove elements from an unevaluated metadata block by key
pruneUnevaluatedMetadata :: String -> CoreExp a -> CoreExp a
pruneUnevaluatedMetadata key expr@(CoreLet _ _) =
  pruneUnevaluatedMetadata key (instantiateLet expr)
pruneUnevaluatedMetadata key (CoreBlock (CoreList items)) =
  CoreBlock (CoreList $ filter keep items)
  where
    keep (CoreList [CorePrim (CoreSymbol k), _]) = k /= key
    keep _ = False
pruneUnevaluatedMetadata _ meta = meta

-- | Precedence classes
precedenceClasses :: [(String, Precedence)]
precedenceClasses =
  [ ("lookup", 100)
  , ("call", 90)
  , ("exp", 85)
  , ("prod", 80)
  , ("sum", 75)
  , ("shift", 60)
  , ("bitwise", 55)
  , ("cmp", 50)
  , ("append", 45)
  , ("map", 42)
  , ("eq", 40)
  , ("bool-prod", 35)
  , ("bool-sum", 30)
  , ("cat", 20)
  , ("meta", 5)
  ]

-- | Use (unevaluated) metadata and default rules to infer fixity and
-- precedence for an operator
determineFixity :: Maybe CoreExpr -> (Fixity, Precedence)
determineFixity (Just meta) = (fixity, fromMaybe 50 prec)
  where
    fixity =
      case readUnevaluatedMetadata "associates" meta symbolName of
        (Just (Just "right")) -> InfixRight
        _ -> InfixLeft
    prec =
      readUnevaluatedMetadata "precedence" meta $ \case
        (CorePrim (CoreInt n)) -> fromInteger n
        (CorePrim (CoreSymbol cls)) -> (fromMaybe 50 (lookup cls precedenceClasses))
        _ -> 50
determineFixity Nothing = (InfixLeft, 50)


-- | Check (unevaluated) metadata for target annotations and their
-- documentation
determineTarget :: CoreExpr -> Maybe (String, String)
determineTarget meta = (, doc) <$> target
  where
    target = join $ readUnevaluatedMetadata "target" meta symbolName
    doc = fromMaybe "" $ join $ readUnevaluatedMetadata "doc" meta stringContent


importsFromMetadata :: ToCoreBindingName a => CoreExp a -> Maybe [Input]
importsFromMetadata m =
  readUnevaluatedMetadata "import" m extract
  where
    extract (CorePrim (CoreString s)) = maybeToList $ parseInputFromString s
    extract (CoreList l) = concatMap extract l
    extract _ = []

pruneImports :: CoreExp a -> CoreExp a
pruneImports = pruneUnevaluatedMetadata "import"
