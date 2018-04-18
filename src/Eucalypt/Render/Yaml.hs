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

import Control.Monad ((>=>))
import qualified Data.ByteString as BS
import Data.Scientific
import Data.Text (Text, pack)
import qualified Text.Libyaml as L
import qualified Data.Yaml as Y
import qualified Data.Yaml.Builder as B
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Eucalypt.Render.Classes
import Data.Conduit
import Conduit

-- | Create primitive Yaml value from 'Primitive'
fromPrimitive :: Primitive -> Y.Value
fromPrimitive p = case p of
  Int i -> Y.Number $ fromInteger i
  Float f -> Y.Number $ fromFloatDigits f
  String s -> Y.String $ pack s
  Symbol s -> Y.String $ pack s



-- | Return text if the expressio is string-like otherwise runtime error
expectText :: CoreExpr -> Interpreter Text
expectText e =
  case e of
    CorePrim (String s) -> return $ pack s
    CorePrim (Symbol s) -> return $ pack s
    _ -> throwEvalError $ LookupKeyNotStringLike e




-- | Monadic equivalent of ToYaml to allow render to drive the
-- evaluation of core
class Monad m => ToMYaml m a where
  toMYaml :: (a -> m a) -> a -> m B.YamlBuilder



instance ToMYaml Interpreter CoreExpr where
  toMYaml _ (CorePrim p) =
    return $
    case p of
      String s -> B.string $ pack s
      Symbol s -> B.string $ pack s
      Int i -> B.scientific $ fromInteger i
      Float f -> B.scientific $ fromFloatDigits f
  toMYaml whnfM (CoreList items) = B.array <$> mapM (toMYaml whnfM) items
  toMYaml whnfM (CoreBlock list) = do
    content <- whnfM list
    case content of
      CoreList items -> B.mapping <$> mapM (whnfM >=> pair) items
      e -> throwEvalError $ BadBlockContent e
    where
      pair item =
        case item of
          (CoreList [k, v]) -> do
            key <- (whnfM >=> expectText) k
            value <- toMYaml whnfM v
            return (key, value)
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
  yield expr .| mapMC (toMYaml whnfM) .| mapC toEvents .| concatC .| sinkList



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
