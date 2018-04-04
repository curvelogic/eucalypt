{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

{-|
Module      : Eucalypt.Render
Description : Dispatch to different renderers
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render (configureRenderer)
  where

import Eucalypt.Driver.Options (EucalyptOptions(..))
import Eucalypt.Render.Classes
import qualified Eucalypt.Render.Yaml as Yaml
import qualified Data.ByteString as BS
import Eucalypt.Core.Syn

-- | Tagged renderer for dispatch to the correct implementation
data DispatchRenderer = YamlRenderer Yaml.YamlConfig | JsonRenderer

instance Renderer DispatchRenderer where
  renderBytes (YamlRenderer config) expr = renderBytes config expr
  renderBytes (JsonRenderer) expr = undefined

-- | Select and configure an appropriate renderer from options
configureRenderer :: EucalyptOptions -> DispatchRenderer
configureRenderer opts = case exportFormat opts of
  Just "yaml" -> YamlRenderer Yaml.YamlConfig {}
  Just "json" -> JsonRenderer
  _ -> YamlRenderer Yaml.YamlConfig {}
