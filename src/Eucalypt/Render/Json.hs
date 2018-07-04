{-|
Module      : Eucalypt.Render.Json
Description : JSON renderer for Eucalypt
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.Json
  where

import Control.Monad ((>=>))
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe
import Data.Text (Text, pack)
import Data.Scientific
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Eucalypt.Render.Classes
import Eucalypt.Render.Common
import qualified Data.ByteString.Lazy as BS

-- | Render key and value to YAML
renderKeyValue :: WhnfEvaluator -> CoreExpr -> CoreExpr -> Interpreter (Maybe (Text, Value))
renderKeyValue whnfM k v = do
  key <- (whnfM >=> expectText) k
  value <- whnfM v
  case value of
    CoreLambda{} -> return Nothing
    CoreOperator{} -> return Nothing
    CoreBuiltin _ -> return Nothing
    CorePAp {} -> return Nothing
    _ -> toJSONExpr whnfM value >>= \rendered -> return (Just (key, rendered))


-- | Json rendering configuration
newtype JsonConfig = JsonConfig
  { jsonPretty :: Bool }


-- | Generate an Aeson Value model of the required JSON in the
--  Interpreter monad.
toJSONExpr :: WhnfEvaluator -> CoreExpr -> Interpreter Value
toJSONExpr _ (CorePrim p) =
  return $
  case p of
    CoreString s -> String $ pack s
    CoreSymbol s -> String $ pack s
    CoreInt i -> Number $ fromInteger i
    CoreFloat f -> Number $ fromFloatDigits f
    CoreBoolean b -> Bool b
    CoreNull -> Null
toJSONExpr whnfM (CoreList items) =
  Array . V.fromList <$> traverse (whnfM >=> toJSONExpr whnfM) items
toJSONExpr whnfM (CoreBlock list) = do
  content <- whnfM list
  case content of
    CoreList items -> Object . HM.fromList . catMaybes <$> traverse (whnfM >=> pair) items
    e -> throwEvalError $ BadBlockContent (CoreExpShow e)
    where
      pair item = do
        i <- boilAwayMetadata whnfM item
        case i of
          Just (CoreList [k, v]) -> renderKeyValue whnfM k v
          Just expr -> throwEvalError $ BadBlockElement (CoreExpShow expr)
          Nothing -> return Nothing
toJSONExpr whnfM (CoreMeta _ v) = toJSONExpr whnfM v
toJSONExpr _ expr = throwEvalError $ NotWeakHeadNormalForm (CoreExpShow expr)


-- | Reduce and render
renderJsonBytes ::
     JsonConfig
  -> WhnfEvaluator
  -> CoreExpr
  -> IO (Either EvaluationError BS.ByteString)
renderJsonBytes cfg whnfM expr =
  case runInterpreter (whnfM expr >>= toJSONExpr whnfM) of
    Left e -> (return . Left) e
    Right j ->
      return $
      Right $
      if jsonPretty cfg
        then encodePretty j
        else encode j


instance Renderer JsonConfig where
  renderBytes config whnfM = fmap (fmap BS.toStrict) . renderJsonBytes config whnfM
