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

import Bound
import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import Data.Scientific
import Data.Text (Text, pack)
import qualified Data.Yaml as Y
import Debug.Trace
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Eucalypt.Render.Classes



-- | Create primitive Yaml value from 'Primitive'
fromPrimitive :: Primitive -> Y.Value
fromPrimitive p = case p of
  Int i -> Y.Number $ fromInteger i
  Float f -> Y.Number $ fromFloatDigits f
  String s -> Y.String $ pack s
  Symbol s -> Y.String $ pack s



-- | Build Yaml output from expression in a non-block context
buildValueYaml :: WhnfEvaluator -> CoreExpr -> Interpreter Y.Value
buildValueYaml whnfM e = do
  expr <- whnfM e
  case expr of
    CorePrim p -> return $ fromPrimitive p
    CoreBlock l -> buildBlockYaml whnfM l
    CoreList items -> Y.array <$> mapM (whnfM >=> buildValueYaml whnfM) items
    CoreLam{} -> return Y.Null
    _ -> Left $ NotWeakHeadNormalForm expr



-- | Build object contents from the list of pairs inside a block wrapper
buildBlockYaml :: WhnfEvaluator -> CoreExpr -> Interpreter Y.Value
buildBlockYaml whnfM list = do
  l <- whnfM list
  case l of
    CoreList items -> Y.object <$> mapM (whnfM >=> pair) items
    e -> Left $ BadBlockContent e
  where
    pair item =
      case item of
        (CoreList [k, v]) -> do
          key <- (whnfM >=> expectText) k
          value <- buildValueYaml whnfM v
          return (key, value)
        e -> Left $ BadBlockElement e



-- | Return text if the expressio is string-like otherwise runtime error
expectText :: CoreExpr -> Interpreter Text
expectText e =
  case e of
    CorePrim (String s) -> Right $ pack s
    CorePrim (Symbol s) -> Right $ pack s
    _ -> Left $ LookupKeyNotStringLike e



-- | Render evaluated content to bytestring
renderYamlBytes :: WhnfEvaluator -> CoreExpr -> Interpreter BS.ByteString
renderYamlBytes whnfM e = Y.encode <$> buildValueYaml whnfM e



-- | Yaml rendering configuration
data YamlConfig = YamlConfig {}



instance Renderer YamlConfig where
  renderBytes _ = renderYamlBytes
