module Eucalypt.Render.Classes
where

import qualified Data.ByteString as BS
import Eucalypt.Core.Syn

data RenderError = RenderError String

class Renderer a where
  renderBytes :: Show b => a -> CoreExp b -> Either RenderError BS.ByteString
