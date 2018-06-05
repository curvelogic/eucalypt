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
import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import Data.Maybe (catMaybes)
import Data.Scientific
import Data.Text (Text, pack)
import qualified Data.Yaml.Builder as B
import Eucalypt.Core.Builtin
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Eucalypt.Render.Classes
import qualified Text.Libyaml as L


-- | Return text if the expressio is string-like otherwise runtime error
expectText :: CoreExpr -> Interpreter Text
expectText e =
  case e of
    CorePrim (CoreString s) -> return $ pack s
    CorePrim (CoreSymbol s) -> return $ pack s
    _ -> throwEvalError $ LookupKeyNotStringLike e




-- | Monadic equivalent of ToYaml to allow render to drive the
-- evaluation of core
class Monad m => ToMYaml m a where
  toMYaml :: (a -> m a) -> a -> m B.YamlBuilder



-- | Get rid of any metadata returning just the expression for the
-- list item or Nothing if the metadata indicated that export should
-- be suppressed.
boilAwayMetadata :: WhnfEvaluator -> CoreExpr -> Interpreter (Maybe CoreExpr)
boilAwayMetadata whnfM (CoreMeta m e) = do
  meta <- whnfM m
  export <- lookupOr whnfM (return (CorePrim (CoreSymbol "copy"))) meta "export"
  if export == CorePrim (CoreSymbol "suppress")
    then return Nothing
    else return $ Just e
boilAwayMetadata _ e = return $ Just e



-- | Render key and value to YAML
renderKeyValue :: WhnfEvaluator -> CoreExpr -> CoreExpr -> Interpreter (Maybe (Text, B.YamlBuilder))
renderKeyValue whnfM k v = do
  key <- (whnfM >=> expectText) k
  value <- whnfM v
  case value of
    CoreLambda{} -> return Nothing
    CoreOperator{} -> return Nothing
    CoreBuiltin _ -> return Nothing
    CorePAp {} -> return Nothing
    _ -> toMYaml whnfM value >>= \rendered -> return (Just (key, rendered))



instance ToMYaml Interpreter CoreExpr where
  toMYaml _ (CorePrim p) =
    return $
    case p of
      CoreString s -> B.string $ pack s
      CoreSymbol s -> B.string $ pack s
      CoreInt i -> B.scientific $ fromInteger i
      CoreFloat f -> B.scientific $ fromFloatDigits f
      CoreBoolean b -> B.bool b
      CoreNull -> B.null
  toMYaml whnfM (CoreList items) = B.array <$> mapM (whnfM >=> toMYaml whnfM) items
  toMYaml whnfM (CoreBlock list) = do
    content <- whnfM list
    case content of
      CoreList items -> B.mapping . catMaybes <$> mapM (whnfM >=> pair) items
      e -> throwEvalError $ BadBlockContent e
    where
      pair item = do
        i <- boilAwayMetadata whnfM item
        case i of
          Just (CoreList [k, v]) -> renderKeyValue whnfM k v
          Just expr -> throwEvalError $ BadBlockElement expr
          Nothing -> return Nothing
  toMYaml whnfM (CoreMeta _ v) = toMYaml whnfM v
  toMYaml _ expr = throwEvalError $ NotWeakHeadNormalForm expr



-- | Direct copy of unexported version in Data.Yaml.Builder
toEvents :: B.YamlBuilder -> [L.Event]
toEvents (B.YamlBuilder front) =
  L.EventStreamStart : L.EventDocumentStart : front [L.EventDocumentEnd, L.EventStreamEnd]



-- | Conduit pipeline that goes as far as events.
--
-- 'encode' needs a 'MonadResource' which needs 'MonadIO' so
-- 'Interpreter' needs a bit of work to be able to take the pipeline
-- all the way to ByteString. For now, we'll stream to events then
-- accumulate them and encode in 'IO'
exprToEventsPipeline :: WhnfEvaluator -> CoreExpr -> ConduitT () Void Interpreter [L.Event]
exprToEventsPipeline whnfM expr =
  yield expr .| mapMC (whnfM >=> toMYaml whnfM) .| mapC toEvents .| concatC .| sinkList



-- | Render expression to list of Yaml events
renderYamlEvents :: WhnfEvaluator -> CoreExpr -> Either EvaluationError [L.Event]
renderYamlEvents whnfM expr =
  runInterpreter $ runConduit $ exprToEventsPipeline whnfM expr



-- | Encoding generated events as bytestring
encodeYamlEvents :: [L.Event] -> IO BS.ByteString
encodeYamlEvents events = runConduitRes $ yieldMany events .| L.encode



-- | Render evaluated content to bytestring
renderYamlBytes :: WhnfEvaluator -> CoreExpr -> IO (Either EvaluationError BS.ByteString)
renderYamlBytes whnfM expr =
  case renderYamlEvents whnfM expr of
    Left e -> (return . Left) e
    Right events -> Right <$> encodeYamlEvents events



-- | Yaml rendering configuration
data YamlConfig = YamlConfig {}



instance Renderer YamlConfig where
  renderBytes _ = renderYamlBytes
