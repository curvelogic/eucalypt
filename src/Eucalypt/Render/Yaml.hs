{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Render.Yaml
Description : YAML renderer for Eucalypt
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.Yaml
  where

import Conduit
import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as C
import Data.Scientific
import Data.Symbol
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Text.Libyaml as L

import Eucalypt.Stg.Event (RenderMetadata(..))
import qualified Eucalypt.Stg.Event as E
import Eucalypt.Stg.Native

tag :: RenderMetadata -> L.Tag -> L.Tag
tag RenderMetadata {metaTag = Nothing} def = def
tag RenderMetadata {metaTag = (Just t)} _ = L.UriTag t

style :: RenderMetadata -> L.Style -> L.Style
style RenderMetadata {metaTag = Nothing} def = def
style RenderMetadata {metaTag = _} _ = L.Plain

-- | Render a native value as a YAML scalar
renderValue :: Native -> E.RenderMetadata -> [L.Event]
renderValue (NativeNumber n) rm =
  case floatingOrInteger n of
    Left r ->
      [ L.EventScalar
          (encodeUtf8 $ pack $ show r)
          (tag rm L.FloatTag)
          (style rm L.PlainNoTag)
          Nothing
      ]
    Right i ->
      [ L.EventScalar
          (encodeUtf8 $ pack $ show i)
          (tag rm L.IntTag)
          (style rm L.PlainNoTag)
          Nothing
      ]
renderValue (NativeSymbol s) rm =
  [ L.EventScalar
      (encodeUtf8 $ pack $ unintern s)
      (tag rm L.StrTag)
      (style rm L.PlainNoTag)
      Nothing
  ]
renderValue (NativeString s) rm =
  [L.EventScalar (encodeUtf8 $ pack s) (tag rm L.NoTag) (textStyle s) Nothing]
  where
    textStyle "" = L.DoubleQuoted
    textStyle "*" = L.DoubleQuoted
    textStyle "/" = L.DoubleQuoted
    textStyle str
      | length str > 60 = L.Literal
    textStyle _ = style rm L.PlainNoTag
renderValue (NativeDynamic _) rm = renderValue (NativeString "**#DYN**") rm

renderBool :: Bool -> [L.Event]
renderBool b =
  [ L.EventScalar
      (encodeUtf8 $
       pack $
       if b
         then "true"
         else "false")
      L.BoolTag
      L.PlainNoTag
      Nothing
  ]

toYamlEvents :: E.Event -> [L.Event]
toYamlEvents e =
  case e of
    E.OutputStreamStart -> [L.EventStreamStart]
    E.OutputStreamEnd -> [L.EventStreamEnd]
    E.OutputDocumentStart -> [L.EventDocumentStart]
    E.OutputDocumentEnd -> [L.EventDocumentEnd]
    E.OutputScalar rm n -> renderValue n rm
    E.OutputNull ->
      [L.EventScalar (encodeUtf8 $ pack "null") L.NullTag L.PlainNoTag Nothing]
    E.OutputTrue -> renderBool True
    E.OutputFalse -> renderBool False
    E.OutputSequenceStart rm ->
      [L.EventSequenceStart (tag rm L.NoTag) L.AnySequence Nothing]
    E.OutputSequenceEnd -> [L.EventSequenceEnd]
    E.OutputMappingStart rm ->
      [L.EventMappingStart (tag rm L.NoTag) L.AnyMapping Nothing]
    E.OutputMappingEnd -> [L.EventMappingEnd]
    _ -> []

renderOptions :: L.FormatOptions
renderOptions = L.setTagRendering L.renderUriTags L.defaultFormatOptions

pipeline :: (MonadIO m, MonadResource m) => ConduitT E.Event Void m BS.ByteString
pipeline = mapC toYamlEvents .| C.concat .| L.encodeWith renderOptions
