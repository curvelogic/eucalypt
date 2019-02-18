{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Source.CsvSource
Description : Ingest CSV into core syntax
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Source.CsvSource where

import Control.Exception.Safe
import Data.ByteString.Lazy (ByteString)
import Data.Csv (HasHeader(..), decode)
import Data.Foldable (toList)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Eucalypt.Core.AnonSyn
import Eucalypt.Source.Error
import Data.Text (pack)
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)

parseCsv :: ByteString -> IO CoreExpr
parseCsv bs =
  let result = decode NoHeader bs :: Either String (Vector [ByteString])
   in case result of
        Right rows ->
          if V.null rows
            then throwM $ FromCsvException "Empty CSV"
            else return . corelist . toList . V.map (record $ V.head rows) $
                 V.tail rows
        Left msg -> throwM $ FromCsvException (pack msg)
  where
    record :: [ByteString] -> [ByteString] -> CoreExpr
    record hdr v = block $ zipWith el hdr v
    el fieldName fieldValue =
      let k = TL.unpack $ decodeUtf8 fieldName
          v = TL.unpack $ decodeUtf8 fieldValue
       in element k $ str v
