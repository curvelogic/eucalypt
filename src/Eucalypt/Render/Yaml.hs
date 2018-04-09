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

import qualified Data.ByteString as BS
import Data.Text ( Text, pack )
import qualified Data.Yaml as Y
import Data.Scientific
import Eucalypt.Core.Syn
import Eucalypt.Core.EvalByName
import Eucalypt.Render.Classes
import Bound

err :: String -> Either RenderError a
err = Left . RenderError

fromPrimitive :: Primitive -> Y.Value
fromPrimitive p = case p of
  Int i -> Y.Number $ fromInteger i
  Float f -> Y.Number $ fromFloatDigits f
  String s -> Y.String $ pack s
  Symbol s -> Y.String $ pack s

buildValueYaml :: Show a => CoreExp a -> Either RenderError Y.Value
buildValueYaml e = case whnf e of
  CorePrim p -> Right $ fromPrimitive p
  CoreBlock l -> buildBlockYaml l
  CoreList items -> (mapM (buildValueYaml . whnf) items) >>= (return . Y.array)
  CoreVar{} -> err "Unexpected Variable (Non-WHNF)"
  CoreLam{} -> err "Unexpected Lambda (Non-WHNF)"
  CoreLet{} -> err "Unexpected Let (Non-WHNF)"
  CoreApp{} -> err "Unexpected Application (Non-WHNF)"
  CoreLookup{} -> err "Unexpected Lookup (Non-WHNF)"

-- | Build object contents from the list of pairs inside a block wrapper
buildBlockYaml :: Show a => CoreExp a -> Either RenderError Y.Value
buildBlockYaml list = case whnf list of
  CoreList items -> (mapM (pair . whnf) items) >>= (return . Y.object)
  e@_ -> err ("Unexpected block contents: " ++ show e)

  where pair item = case item of
          (CoreList (k:v:[])) -> do key <- (expectText k)
                                    value <- buildValueYaml v
                                    return (key, value)
          e@_ -> err ("Unexpected block item: " ++ show e)

expectText :: Show a => CoreExp a -> Either RenderError Text
expectText e = case whnf e of
  CorePrim (String s) -> Right $ pack s
  CorePrim (Symbol s) -> Right $ pack s
  e -> err ("Non-string-like key" ++ show e)


renderYamlBytes :: Show a => CoreExp a -> Either RenderError BS.ByteString
renderYamlBytes e = buildValueYaml e >>= (return . Y.encode)

-- | Yaml rendering configuration
data YamlConfig = YamlConfig {}

instance Renderer YamlConfig where
  renderBytes _ = renderYamlBytes
