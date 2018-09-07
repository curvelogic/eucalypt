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
import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import qualified Data.Conduit.Combinators as C
import Data.Maybe (catMaybes)
import Data.Scientific
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Yaml.Builder as B
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Eucalypt.Render.Classes
import Eucalypt.Render.Common
import qualified Text.Libyaml as L

import qualified Eucalypt.Stg.Event as E
import Eucalypt.Stg.Syn (Native(..))



-- | Monadic equivalent of ToYaml to allow render to drive the
-- evaluation of core
class Monad m => ToMYaml m a where
  toMYaml :: (a -> m a) -> a -> m B.YamlBuilder




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
      e -> throwEvalError $ BadBlockContent (CoreExpShow e)
    where
      pair item = do
        i <- boilAwayMetadata whnfM item
        case i of
          Just (CoreList [k, v]) -> renderKeyValue whnfM k v
          Just expr -> throwEvalError $ BadBlockElement (CoreExpShow expr)
          Nothing -> return Nothing
  toMYaml whnfM (CoreMeta _ v) = toMYaml whnfM v
  toMYaml _ expr = throwEvalError $ NotWeakHeadNormalForm (CoreExpShow expr)



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

-- STG implementation

renderValue :: Native -> L.Event
renderValue (NativeNumber n) =
  case floatingOrInteger n of
    Left r -> L.EventScalar (encodeUtf8 $ pack $ show r) L.FloatTag L.PlainNoTag Nothing
    Right i -> L.EventScalar (encodeUtf8 $ pack $ show i) L.IntTag L.PlainNoTag Nothing
renderValue (NativeSymbol s) =
  L.EventScalar (encodeUtf8 $ pack s) L.StrTag L.PlainNoTag Nothing
renderValue (NativeString s) =
  L.EventScalar (encodeUtf8 $ pack s) L.StrTag L.PlainNoTag Nothing
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
