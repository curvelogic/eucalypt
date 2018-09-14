{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Conduit
import Control.Monad ((>=>))
import Control.Monad.State
import Data.Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Scientific
import qualified Data.Vector as V
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Eucalypt.Render.Classes
import Eucalypt.Render.Common
import qualified Eucalypt.Stg.Event as E
import Eucalypt.Stg.Syn (Native(..))


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
  -> IO (Either EvaluationError BL.ByteString)
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
  renderBytes config whnfM = fmap (fmap BL.toStrict) . renderJsonBytes config whnfM

-- | STG implementation

data JSONContext = InArray | InObject | InPair

data JSONFormatState = JSONFormatState
  { lastEvent :: Maybe E.Event -- ^ to determine when to insert ','
  , context :: [JSONContext] -- ^ to determine when to insert ':'
  , output :: BS.ByteString -- ^ to accumulate
  }

initState :: JSONFormatState
initState = JSONFormatState Nothing [InArray] ""

setLast :: MonadState JSONFormatState m => E.Event -> m ()
setLast e = modify (\s -> s { lastEvent = Just e})

getLast :: MonadState JSONFormatState m => m (Maybe E.Event)
getLast = gets lastEvent

pushContext :: MonadState JSONFormatState m => JSONContext -> m ()
pushContext c =
  modify (\s@JSONFormatState {context = oc} -> s {context = c : oc})

popContext :: MonadState JSONFormatState m => m ()
popContext = do
  (_:t) <- gets context
  modify $ \s -> s {context = t}

currentContext :: MonadState JSONFormatState m => m JSONContext
currentContext = head <$> gets context

putText :: MonadState JSONFormatState m => BS.ByteString -> m ()
putText t = modify $ \s -> s {output = BS.append (output s) t}

putScalar :: MonadState JSONFormatState m => E.Event -> BS.ByteString -> m ()
putScalar e txt = do
  c <- currentContext
  case c of
    InPair -> do
      popContext
      putText $ BS.concat [": ", txt]
    InArray -> do
      pre <- getLast
      putText $
        case pre of
          Nothing -> txt
          Just E.OutputSequenceStart -> txt
          _ -> BS.concat [", ", txt]
    InObject -> do
      pre <- getLast
      pushContext InPair
      putText $
        case pre of
          Nothing -> txt
          Just E.OutputMappingStart -> txt
          _ -> BS.concat [", ", txt]
  setLast e

formatScalar :: Native -> BS.ByteString
formatScalar (NativeNumber n) =
  case floatingOrInteger n of
    Left r -> encodeUtf8 $ pack $ show r
    Right i -> encodeUtf8 $ pack $ show i
formatScalar (NativeSymbol s) = encodeUtf8 $ pack $ show s
formatScalar (NativeString s) = encodeUtf8 $ pack $ show s
formatScalar (NativeBool b) =
  if b
    then "true"
    else "false"

putBSFragment :: MonadState JSONFormatState m => E.Event -> m ()
putBSFragment e@E.OutputSequenceStart = do
  c <- currentContext
  case c of
    InPair -> do
      popContext
      putText ": ["
    InArray -> do
      pre <- getLast
      putText $
        case pre of
          Nothing -> "["
          Just E.OutputSequenceStart -> "["
          _ -> ", ["
    InObject -> do
      pre <- getLast
      putText $
        case pre of
          Nothing -> "["
          Just E.OutputMappingStart -> "["
          _ -> ", ["
  setLast e
  pushContext InArray

putBSFragment e@E.OutputSequenceEnd = setLast e >> popContext >> putText "]"

putBSFragment e@E.OutputMappingStart = do
  c <- currentContext
  case c of
    InPair -> do
      popContext
      putText ": {"
    InArray -> do
      pre <- getLast
      putText $
        case pre of
          Nothing -> "{"
          Just E.OutputSequenceStart -> "{"
          _ -> ", {"
    InObject -> do
      pre <- getLast
      putText $
        case pre of
          Nothing -> "{"
          Just E.OutputMappingStart -> "{"
          _ -> ", {"
  setLast e
  pushContext InObject

putBSFragment e@E.OutputMappingEnd = setLast e >> popContext >> putText "}"
putBSFragment e@(E.OutputScalar n) = putScalar e (formatScalar n)
putBSFragment e@E.OutputNull = putScalar e "null"
putBSFragment _ = putText ""

runFormatter :: State JSONFormatState () -> BS.ByteString
runFormatter = output . (`execState` initState)

pipeline :: Monad m => ConduitT E.Event Void m BS.ByteString
pipeline = mapC putBSFragment .| runFormatter <$> foldlC (>>) (return ())
