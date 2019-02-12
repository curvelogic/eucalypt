{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.GlobalInfo
Description : GlobalInfo type for describing globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.GlobalInfo
  ( globalNames
  , globalIndexes
  , gref
  , globalSignature
  , Strictness(..)
  , module Data.Symbol
  ) where

import qualified Data.Map as M
import Data.Symbol
import Eucalypt.Stg.Vec (Reference(..))

data Strictness = Strict | NonStrict
  deriving (Show, Eq)

data GlobalInfo = GlobalInfo
  { globalName :: Symbol
  , globalStrictness :: [Strictness]
  }

globalRegistry :: [GlobalInfo]
globalRegistry =
  [ GlobalInfo "EQ" [Strict, Strict]
  , GlobalInfo "TRUE" []
  , GlobalInfo "FALSE" []
  , GlobalInfo "NOT" [Strict]
  , GlobalInfo "AND" [Strict, NonStrict]
  , GlobalInfo "OR" [Strict, NonStrict]
  , GlobalInfo "IF" [Strict, NonStrict, NonStrict]
  , GlobalInfo "CONS" [NonStrict, NonStrict]
  , GlobalInfo "NIL" [NonStrict]
  , GlobalInfo "HEAD" [NonStrict]
  , GlobalInfo "TAIL" [NonStrict]
  , GlobalInfo "CONCAT" [NonStrict]
  , GlobalInfo "REVERSE" [NonStrict]
  , GlobalInfo "PANIC" [Strict]
  , GlobalInfo "!KEYNOTFOUND" [Strict]
  , GlobalInfo "BOMB" []
  , GlobalInfo "CAT" [NonStrict, NonStrict]
  , GlobalInfo "BLOCK" [NonStrict]
  , GlobalInfo "ELEMENTS" [NonStrict]
  , GlobalInfo "MERGE" [NonStrict, NonStrict]
  , GlobalInfo "DEEPMERGE" [NonStrict, NonStrict]
  , GlobalInfo "DEEPMERGEIFBLOCKS" [NonStrict, NonStrict]
  , GlobalInfo "LOOKUP" [NonStrict, Strict]
  , GlobalInfo "LOOKUPLIST" [NonStrict, Strict]
  , GlobalInfo "LOOKUPOR" [NonStrict, Strict, NonStrict]
  , GlobalInfo "LOOKUPLISTOR" [NonStrict, Strict, NonStrict]
  , GlobalInfo "META" [Strict]
  , GlobalInfo "WITHMETA" [NonStrict, NonStrict]
  , GlobalInfo "KNIL" []
  , GlobalInfo "KEMPTYBLOCK" []
  , GlobalInfo "seqNatList" [NonStrict]
  , GlobalInfo "seqPairList" [NonStrict]
  , GlobalInfo "ADD" [Strict, Strict]
  , GlobalInfo "SUB" [Strict, Strict]
  , GlobalInfo "MUL" [Strict, Strict]
  , GlobalInfo "DIV" [Strict, Strict]
  , GlobalInfo "MOD" [Strict, Strict]
  , GlobalInfo "FLOOR" [Strict]
  , GlobalInfo "CEILING" [Strict]
  , GlobalInfo "LT" [Strict, Strict]
  , GlobalInfo "GT" [Strict, Strict]
  , GlobalInfo "LTE" [Strict, Strict]
  , GlobalInfo "GTE" [Strict, Strict]
  , GlobalInfo "Emit.suppresses" [NonStrict]
  , GlobalInfo "Emit.renderKV" [NonStrict]
  , GlobalInfo "Emit.continueKVList" [NonStrict]
  , GlobalInfo "Emit.startList" [NonStrict, NonStrict]
  , GlobalInfo "Emit.continueList" [NonStrict]
  , GlobalInfo "Emit.forceExportMetadata" [NonStrict]
  , GlobalInfo "RENDER" [NonStrict]
  , GlobalInfo "NULL" []
  , GlobalInfo "NUMPARSE" [Strict]
  , GlobalInfo "MATCHES" [Strict, Strict]
  , GlobalInfo "MATCH" [Strict, Strict]
  , GlobalInfo "JOIN" [Strict, Strict]
  , GlobalInfo "SPLIT" [Strict, Strict]
  , GlobalInfo "STR" [Strict]
  , GlobalInfo "SYM" [Strict]
  , GlobalInfo "LETTERS" [Strict]
  , GlobalInfo "DQ" []
  , GlobalInfo "FMT" [Strict, Strict]
  , GlobalInfo "UPPER" [Strict]
  , GlobalInfo "LOWER" [Strict]
  , GlobalInfo "IFIELDS" [Strict]
  , GlobalInfo "IOHM.EMPTY" []
  , GlobalInfo "IOHM.INSERT" [Strict, Strict, Strict]
  , GlobalInfo "IOHM.WRAP" [NonStrict]
  , GlobalInfo "IOHM.LIST" [Strict]
  , GlobalInfo "IOHM.LOOKUP" [Strict, Strict]
  , GlobalInfo "IOHM.LOOKUPOR" [Strict, Strict, NonStrict]
  , GlobalInfo "IOHM.UNWRAP" [Strict]
  , GlobalInfo "IOHM.MERGE" [Strict, Strict]
  , GlobalInfo "IOHM.MERGEWITH" [Strict, Strict, Strict]
  , GlobalInfo "IOHMBLOCK.DEEPMERGE" [Strict, Strict]
  , GlobalInfo "IOHMBLOCK.DEEPMERGEIFBLOCKS" [Strict, Strict]
  , GlobalInfo "IOHM.EQ" [Strict, Strict]
  , GlobalInfo "IOSM.EMPTY" []
  , GlobalInfo "IOSM.INSERT" [Strict, Strict, Strict]
  , GlobalInfo "IOSM.WRAP" [NonStrict]
  , GlobalInfo "IOSM.LIST" [Strict]
  , GlobalInfo "IOSM.LOOKUP" [Strict, Strict]
  , GlobalInfo "IOSM.LOOKUPOR" [Strict, Strict, NonStrict]
  , GlobalInfo "IOSM.UNWRAP" [Strict]
  , GlobalInfo "IOSM.MERGE" [Strict, Strict]
  , GlobalInfo "IOSM.MERGEWITH" [Strict, Strict, Strict]
  , GlobalInfo "IOSMBLOCK.DEEPMERGE" [Strict, Strict]
  , GlobalInfo "IOSMBLOCK.DEEPMERGEIFBLOCKS" [Strict, Strict]
  , GlobalInfo "IOSM.EQ" [Strict, Strict]
  ]

globalIndexes :: M.Map Symbol Int
globalIndexes = foldl add M.empty $ zip globalRegistry [0 ..]
  where
    add m (GlobalInfo {..}, i) =
      M.insert globalName i m

globalNames :: [Symbol]
globalNames = map globalName globalRegistry

globalSignature :: String -> [Strictness]
globalSignature = (smap M.!) . intern
  where
    add m GlobalInfo {..} =
      M.insert globalName globalStrictness m
    smap = foldl add M.empty globalRegistry

-- | Retrieve a reference suitable for pointing into the runtime
-- global environment.
gref :: String -> Reference a
gref name = G $ fromIntegral $ globalIndexes M.! intern name
