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
import qualified Eucalypt.Render.Json as Json

-- | Tagged renderer for dispatch to the correct implementation
data DispatchRenderer
  = YamlRenderer Yaml.YamlConfig
  | JsonRenderer Json.JsonConfig

instance Renderer DispatchRenderer where
  renderBytes (YamlRenderer config) expr = renderBytes config expr
  renderBytes (JsonRenderer config) expr = renderBytes config expr

-- | Select and configure an appropriate renderer from options
configureRenderer :: EucalyptOptions -> DispatchRenderer
configureRenderer opts = case optionExportFormat opts of
  Just "yaml" -> YamlRenderer Yaml.YamlConfig {}
  Just "json" -> JsonRenderer Json.JsonConfig { Json.jsonPretty = True }
  _ -> YamlRenderer Yaml.YamlConfig {}
