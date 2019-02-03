{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Foldable (toList)
import qualified Data.Map.Strict as MS
import Data.Scientific
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
    Left r -> [L.EventScalar (encodeUtf8 $ pack $ show r) (tag rm L.FloatTag) (style rm L.PlainNoTag) Nothing]
    Right i -> [L.EventScalar (encodeUtf8 $ pack $ show i) (tag rm L.IntTag) (style rm L.PlainNoTag) Nothing]
renderValue (NativeSymbol s) rm =
  [L.EventScalar (encodeUtf8 $ pack s) (tag rm L.StrTag) (style rm L.PlainNoTag) Nothing]
renderValue (NativeString s) rm =
  [L.EventScalar (encodeUtf8 $ pack s) (tag rm L.NoTag) (textStyle s) Nothing]
  where
    textStyle "" = L.DoubleQuoted
    textStyle "*" = L.DoubleQuoted
    textStyle "/" = L.DoubleQuoted
    textStyle str
      | length str > 60 = L.Literal
    textStyle _ = style rm L.PlainNoTag
renderValue (NativeBool b) rm =
  [L.EventScalar
    (encodeUtf8 $
     pack $
     if b
       then "true"
       else "false")
    (tag rm L.BoolTag)
    (style rm L.PlainNoTag)
    Nothing]
renderValue (NativeSet s) _ =
  [L.EventSequenceStart L.NoTag L.AnySequence Nothing] ++
  concatMap (`renderValue` RenderMetadata {metaTag = Nothing}) (toList s) ++
  [L.EventSequenceEnd]
renderValue (NativeDict d) _ =
  [L.EventMappingStart L.NoTag L.AnyMapping Nothing] ++
  concatMap kv (MS.assocs d) ++ [L.EventMappingEnd]
  where
    kv (k, v) =
      renderValue k RenderMetadata {metaTag = Nothing} ++
      renderValue v RenderMetadata {metaTag = Nothing}

toYamlEvents :: E.Event -> [L.Event]
toYamlEvents e =
  case e of
    E.OutputStreamStart -> [L.EventStreamStart]
    E.OutputStreamEnd -> [L.EventStreamEnd]
    E.OutputDocumentStart -> [L.EventDocumentStart]
    E.OutputDocumentEnd -> [L.EventDocumentEnd]
    E.OutputScalar rm n -> renderValue n rm
    E.OutputNull -> [L.EventScalar (encodeUtf8 $ pack "null") L.NullTag L.PlainNoTag Nothing]
    E.OutputSequenceStart -> [L.EventSequenceStart L.NoTag L.AnySequence Nothing]
    E.OutputSequenceEnd -> [L.EventSequenceEnd]
    E.OutputMappingStart -> [L.EventMappingStart L.NoTag L.AnyMapping Nothing]
    E.OutputMappingEnd -> [L.EventMappingEnd]
    _ -> []

pipeline :: (MonadIO m, MonadResource m) => ConduitT E.Event Void m BS.ByteString
pipeline = mapC toYamlEvents .| C.concat .| L.encode
