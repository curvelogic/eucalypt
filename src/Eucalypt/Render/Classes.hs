module Eucalypt.Render.Classes
where

import qualified Data.ByteString as BS
import Eucalypt.Core.Syn
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Error

class Renderer r where
  renderBytes :: r -> WhnfEvaluator -> CoreExpr -> Interpreter BS.ByteString
