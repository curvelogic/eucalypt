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
  toMYaml whnfM (CoreList items) = B.array <$> mapM (toMYaml whnfM) items
  toMYaml whnfM (CoreBlock list) = do
    content <- whnfM list
    case content of
      CoreList items -> B.mapping . catMaybes <$> mapM (whnfM >=> pair) items
      e -> throwEvalError $ BadBlockContent e
    where
      pair item =
        case item of
          (CoreList [k, v]) -> do
            key <- (whnfM >=> expectText) k
            value <- whnfM v
            case value of
              CoreLam _ -> return Nothing
              _ ->  toMYaml whnfM value >>= \rendered -> return (Just (key, rendered))
          expr -> throwEvalError $ BadBlockElement expr
  toMYaml _ (CoreLam _) = return B.null
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
-- accumalate them and encode in 'IO'
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
