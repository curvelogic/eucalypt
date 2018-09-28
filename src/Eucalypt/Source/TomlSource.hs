{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Source.TomlSource
Description : Ingest TOML into core syntax
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Source.TomlSource where

import Control.Exception.Safe
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Eucalypt.Core.Syn
import Eucalypt.Source.Error
import qualified Toml

-- | Convert a TOML primitive to a core expression
--
tomlValue :: Toml.Value t -> CoreExpr
tomlValue (Toml.Bool b) = corebool b
tomlValue (Toml.Integer n) = int n
tomlValue (Toml.Double d) = float d
tomlValue (Toml.Text s) = (str . unpack) s
tomlValue (Toml.Date d) =
  withMeta
    (block [element "toml" $ block [element "type" $ sym "date"]])
    (str $ show d)
tomlValue (Toml.Array a) = CoreList $ map tomlValue a

pieceToBindingName :: Toml.Piece -> CoreBindingName
pieceToBindingName = unpack . Toml.unPiece

keyToBindingName :: Toml.Key -> CoreBindingName
keyToBindingName (Toml.Key k) =
  intercalate "." $ map pieceToBindingName (NonEmpty.toList k)

-- | Translate a prefix tree into a list of blocks that can be
-- concatenated
translatePrefixTree :: Toml.PrefixTree Toml.TOML -> CoreExpr
translatePrefixTree (Toml.Leaf k a) =
  inPrefixBlocks k $ translateToml a
translatePrefixTree Toml.Branch {..} =
  inPrefixBlocks bCommonPref $ translatePrefixMap bPrefixMap

-- | Translate a prefix map
translatePrefixMap :: Toml.PrefixMap Toml.TOML -> CoreExpr
translatePrefixMap m =
  block
    [ element (pieceToBindingName k) $ translatePrefixTree v
    | (k, v) <- HM.toList m
    ]

-- | Return expression wrapped in enough blocks to represent the
-- prefix
inPrefixBlocks :: Toml.Prefix -> CoreExpr -> CoreExpr
inPrefixBlocks k ex = foldr wrap ex names
  where
    wrap l r = block [element l r]
    names = map pieceToBindingName (NonEmpty.toList . Toml.unKey $ k)

-- | Translate a TOML file into a 'CoreExpr'
--
translateToml :: Toml.TOML -> CoreExpr
translateToml Toml.TOML {..} = foldl1 collapse (pairBlocks ++ tables)
  where
    pairBlocks = map kvBlock $ HM.toList tomlPairs
    tables = map translatePrefixTree $ HM.elems tomlTables
    kvBlock (k, Toml.AnyValue val) = inPrefixBlocks k $ tomlValue val
    collapse (CoreBlock (CoreList l)) (CoreBlock (CoreList r)) =
      CoreBlock . CoreList $ l ++ r
    collapse _ _ = error "Collapsing non-block expressions"

-- | Parse inert TOML data into a CoreExpr
parseTomlData :: BS.ByteString -> IO CoreExpr
parseTomlData src =
  case Toml.parse . decodeUtf8 $ src of
    Left (Toml.ParseException t) -> throwM $ FromTomlException t
    Right val -> return . translateToml $ val
