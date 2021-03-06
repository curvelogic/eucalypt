{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Render.Text
Description : Line-based text rendering
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.Text
  where

import Conduit
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (Builder, stringUtf8)
import Data.Maybe (maybeToList)
import Data.Scientific
import Data.Symbol
import qualified Eucalypt.Stg.Event as E
import Eucalypt.Stg.Native


formatScalar :: Native -> Builder
formatScalar (NativeNumber n) =
  case floatingOrInteger n of
    Left r -> stringUtf8 $ show r
    Right i -> stringUtf8 $ show i
formatScalar (NativeSymbol s) = stringUtf8 $ unintern s
formatScalar (NativeString s) = stringUtf8 s
formatScalar (NativeDynamic _) = "**#DYN**"

toFragment :: E.Event -> Maybe Builder
toFragment (E.OutputScalar _ n) = Just $ formatScalar n
toFragment E.OutputTrue = Just $ stringUtf8 "true"
toFragment E.OutputFalse = Just $ stringUtf8 "false"
toFragment _ = Nothing

pipeline :: Monad m => ConduitT E.Event Void m BS.ByteString
pipeline =
  mapC toFragment .| concatMapC maybeToList .| intersperseC (stringUtf8 "\n") .|
  BL.toStrict <$>
  sinkLazyBuilder
