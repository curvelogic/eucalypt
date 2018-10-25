{-|
Module      : Eucalypt.Source.TextSource
Description : Ingest text lines into core syntax
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Source.TextSource where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Eucalypt.Core.SourceMap
import Eucalypt.Core.Syn

parseTextLines :: BS.ByteString -> IO CoreExpr
parseTextLines =
  return . anon corelist . map (anon str . T.unpack) . T.lines . decodeUtf8
