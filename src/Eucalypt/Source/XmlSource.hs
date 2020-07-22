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
import Data.Char (isSpace)

-- | Parse XML into CoreExpr
parseXml :: BS.ByteString -> IO CoreExpr
parseXml =
  maybe (throwM FromXmlException) (return . elementToHiccupCore) . parseXMLDoc

fromQName :: QName -> String
fromQName = qName

attrToBlockElement :: Attr -> CoreExpr
attrToBlockElement Attr{..} =
  element (fromQName attrKey) $ str attrVal

nonWhiteSpace :: Content -> Bool
nonWhiteSpace (Text s) = not $ all isSpace $ cdData s
nonWhiteSpace _ = True

elementToHiccupCore :: Element -> CoreExpr
elementToHiccupCore Element {..} =
  corelist $
  (sym . fromQName $ elName) :
  block (map attrToBlockElement elAttribs) :
  map contentToCore (filter nonWhiteSpace elContent)

contentToCore :: Content -> CoreExpr
contentToCore (Elem e) = elementToHiccupCore e
contentToCore (Text cdata) = str . cdData $ cdata
contentToCore (CRef s) = str s
