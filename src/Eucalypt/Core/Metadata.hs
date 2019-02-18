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
import Data.Bifunctor (bimap)
import Data.Either (partitionEithers)
import Data.Maybe
import Eucalypt.Syntax.Input
import Eucalypt.Core.Syn


-- | Core metadata values must be blocks (or at least block-valued
-- lets) - this is used during desugar to allow various shortcuts in
-- the surface syntax. At present it's very ad hoc but we'll
-- substitute a more general handling later
normaliseMetadata :: CoreExpr -> CoreExpr
normaliseMetadata s@(CorePrim smid (CoreString _)) = block smid [element smid "doc" s]
normaliseMetadata e@(CorePrim smid (CoreSymbol s))
  | s == "alias" = block smid [element smid "ref" e]
  | s == "suppress" = block smid [element smid "export" e]
  | s == "main" = block smid [element smid "target" e]
  | s == "trace" = block smid [element smid "trace" (CorePrim smid (CoreBoolean True))]
  | otherwise = block smid []
normaliseMetadata e = e

-- | Some annotation metadata can be expressed at the declaration
-- level but should apply to the value. This splits declaration
-- metadata into those that apply to declaration and those that apply
-- to the value
splitAnnotationMetadata :: CoreExpr -> (Maybe CoreExpr, Maybe CoreExpr)
splitAnnotationMetadata m@CoreLet{} =
  splitAnnotationMetadata $ instantiateLet m
splitAnnotationMetadata (CoreBlock smid (CoreList _ items)) =
  bimap maybeBlock maybeBlock $ partitionEithers $ map classify items
  where
    classify item@(CoreList _ [CorePrim _ (CoreSymbol k), _]) =
      if k == "import"
        then Right item
        else Left item
    classify item = Left item
    maybeBlock :: [CoreExpr] -> Maybe CoreExpr
    maybeBlock els =
      if null els
        then Nothing
        else (Just . CoreBlock smid . CoreList smid) els
splitAnnotationMetadata m = (Just m, Nothing)

-- | Read from unevaluated metadata (expanding out only an outer let
-- to prepare a block for lookup).
readUnevaluatedMetadata :: String -> CoreExp a -> (CoreExp a -> b) -> Maybe b
readUnevaluatedMetadata key expr@CoreLet{} readVal =
  readUnevaluatedMetadata key (instantiateLet expr) readVal
readUnevaluatedMetadata key (CoreBlock _ (CoreList _ items)) readVal =
  readVal <$> lookup key buildSearchList
  where
    buildSearchList = mapMaybe kv items
    kv (CoreList _ [CorePrim _ (CoreSymbol k), v]) = Just (k, v)
    kv (CoreMeta _ _ i) = kv i
    kv _ = Nothing
readUnevaluatedMetadata _ _ _ = Nothing

-- | Remove elements from an unevaluated metadata block by key
pruneUnevaluatedMetadata :: String -> CoreExp a -> CoreExp a
pruneUnevaluatedMetadata key expr@CoreLet{} =
  pruneUnevaluatedMetadata key (instantiateLet expr)
pruneUnevaluatedMetadata key (CoreBlock bsmid (CoreList lsmid items)) =
  CoreBlock bsmid (CoreList lsmid $ filter keep items)
  where
    keep (CoreList _ [CorePrim _ (CoreSymbol k), _]) = k /= key
    keep _ = False
pruneUnevaluatedMetadata _ meta = meta

-- | Precedence classes
precedenceClasses :: [(String, Precedence)]
precedenceClasses =
  [ ("lookup", 90)
  , ("call", 90)
  , ("bool-unary", 88)
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
  , ("apply", 10)
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
        (CorePrim _ (CoreInt n)) -> fromInteger n
        (CorePrim _ (CoreSymbol cls)) -> (fromMaybe 50 (lookup cls precedenceClasses))
        _ -> 50
determineFixity Nothing = (InfixLeft, 50)

-- | Determine precedence when fixity is already known (i.e. unary).
determinePrecedence :: Maybe CoreExpr -> Precedence
determinePrecedence (Just meta) =
  fromMaybe 50 $
  readUnevaluatedMetadata "precedence" meta $ \case
    (CorePrim _ (CoreInt n)) -> fromInteger n
    (CorePrim _ (CoreSymbol cls)) -> (fromMaybe 50 (lookup cls precedenceClasses))
    _ -> 50
determinePrecedence _ = 50

-- | Check (unevaluated) metadata for target annotations and their
-- documentation
determineTarget :: CoreExpr -> Maybe (String, String, Maybe String)
determineTarget meta = (, doc, format) <$> target
  where
    target = join $ readUnevaluatedMetadata "target" meta symbolName
    doc = fromMaybe "" $ join $ readUnevaluatedMetadata "doc" meta stringContent
    format = join $ readUnevaluatedMetadata "format" meta symbolName



importsFromMetadata :: CoreExp a -> Maybe [Input]
importsFromMetadata m =
  readUnevaluatedMetadata "import" m extract
  where
    extract (CorePrim _ (CoreString s)) = maybeToList $ parseInputFromString s
    extract (CoreList _ l) = concatMap extract l
    extract _ = []

pruneImports :: CoreExp a -> CoreExp a
pruneImports = pruneUnevaluatedMetadata "import"
