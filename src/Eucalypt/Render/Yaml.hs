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
import Data.Scientific
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Text.Libyaml as L

import qualified Eucalypt.Stg.Event as E
import Eucalypt.Stg.Syn (Native(..))

-- STG implementation

renderValue :: Native -> L.Event
renderValue (NativeNumber n) =
  case floatingOrInteger n of
    Left r -> L.EventScalar (encodeUtf8 $ pack $ show r) L.FloatTag L.PlainNoTag Nothing
    Right i -> L.EventScalar (encodeUtf8 $ pack $ show i) L.IntTag L.PlainNoTag Nothing
renderValue (NativeSymbol s) =
  L.EventScalar (encodeUtf8 $ pack s) L.StrTag L.PlainNoTag Nothing
renderValue (NativeString s) =
  L.EventScalar (encodeUtf8 $ pack s) L.NoTag (style s) Nothing
  where
    style "" = L.DoubleQuoted
    style "*" = L.DoubleQuoted
    style "/" = L.DoubleQuoted
    style str | length str > 60 = L.Literal
    style _ = L.PlainNoTag
renderValue (NativeBool b) =
  L.EventScalar
    (encodeUtf8 $
     pack $
     if b
       then "true"
       else "false")
    L.BoolTag
    L.PlainNoTag
    Nothing


toYamlEvents :: E.Event -> [L.Event]
toYamlEvents e =
  case e of
    E.OutputStreamStart -> [L.EventStreamStart]
    E.OutputStreamEnd -> [L.EventStreamEnd]
    E.OutputDocumentStart -> [L.EventDocumentStart]
    E.OutputDocumentEnd -> [L.EventDocumentEnd]
    E.OutputScalar n -> [renderValue n]
    E.OutputNull -> [L.EventScalar (encodeUtf8 $ pack "null") L.NullTag L.PlainNoTag Nothing]
    E.OutputSequenceStart -> [L.EventSequenceStart Nothing]
    E.OutputSequenceEnd -> [L.EventSequenceEnd]
    E.OutputMappingStart -> [L.EventMappingStart Nothing]
    E.OutputMappingEnd -> [L.EventMappingEnd]
    _ -> []

pipeline :: (MonadIO m, MonadResource m) => ConduitT E.Event Void m BS.ByteString
pipeline = mapC toYamlEvents .| C.concat .| L.encode
