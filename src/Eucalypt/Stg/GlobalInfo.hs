{-# LANGUAGE RecordWildCards #-}
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
  ) where

import qualified Data.HashMap.Strict as HM
import Eucalypt.Stg.Vec (Reference(..))

data Strictness = Strict | NonStrict
  deriving (Show, Eq)

data GlobalInfo = GlobalInfo
  { globalName :: String
  , globalStrictness :: [Strictness]
  }

globalRegistry :: [GlobalInfo]
globalRegistry =
  [ GlobalInfo "EQ" [NonStrict, NonStrict]
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
  , GlobalInfo "Emit.emptyList" []
  , GlobalInfo "Emit.startList" [NonStrict, NonStrict]
  , GlobalInfo "Emit.continueList" [NonStrict]
  , GlobalInfo "Emit.wrapBlock" [NonStrict]
  , GlobalInfo "Emit.forceExportMetadata" [NonStrict]
  , GlobalInfo "Emit.forceExportMetadataKVList" [NonStrict]
  , GlobalInfo "Emit.forceKVNatPair" [NonStrict]
  , GlobalInfo "Emit.isRenderMetadataKey" [Strict]
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
  , GlobalInfo "IOHM.UNWRAP" [Strict]
  , GlobalInfo "IOHM.MERGE" [Strict, Strict]
  , GlobalInfo "IOHM.MERGEWITH" [Strict, Strict, Strict]
  , GlobalInfo "IOHMBLOCK.DEEPMERGE" [Strict, Strict]
  , GlobalInfo "IOHMBLOCK.DEEPMERGEIFBLOCKS" [Strict, Strict]
  , GlobalInfo "IOHM.EQ" [Strict, Strict]
  ]

globalIndexes :: HM.HashMap String Int
globalIndexes = foldl add HM.empty $ zip globalRegistry [0 ..]
  where
    add m (GlobalInfo {..}, i) = HM.insert globalName i m

globalNames :: [String]
globalNames = map globalName globalRegistry

globalSignature :: String -> [Strictness]
globalSignature = (smap HM.!)
  where
    add m GlobalInfo {..} = HM.insert globalName globalStrictness m
    smap = foldl add HM.empty globalRegistry

-- | Retrieve a reference suitable for pointing into the runtime
-- global environment.
gref :: String -> Reference a
gref name = G $ fromIntegral $ globalIndexes HM.! name
