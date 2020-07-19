{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Source.XmlSource
Description : Ingest XML source into core syntax
Copyright   : (c) Greg Hawkins, 2020
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Source.XmlSource where

import Control.Exception.Safe
import qualified Data.ByteString as BS
import Eucalypt.Core.AnonSyn
import Eucalypt.Source.Error
import Text.XML.Light (Element(..), QName(..), Attr(..), Content(..), CData(..))
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Lexer ()

-- | Parse XML into CoreExpr
parseXml :: BS.ByteString -> IO CoreExpr
parseXml =
  maybe (throwM FromXmlException) (return . elementToCore) . parseXMLDoc

fromQName :: QName -> String
fromQName = qName

attrToBlockElement :: Attr -> CoreExpr
attrToBlockElement Attr{..} =
  element (fromQName attrKey) $ str attrVal

elementToCore :: Element -> CoreExpr
elementToCore Element {..} =
  block
    [ element "_tag" $ str . fromQName $ elName
    , element "_attrs" $ block $ map attrToBlockElement elAttribs
    , element "_content" $ corelist $ map contentToCore elContent
    ]

contentToCore :: Content -> CoreExpr
contentToCore (Elem e) = elementToCore e
contentToCore (Text cdata) = str . cdData $ cdata
contentToCore (CRef s) = str s
