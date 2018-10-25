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
import Control.Monad.State
import qualified Data.ByteString as BS
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Scientific
import qualified Eucalypt.Stg.Event as E
import Eucalypt.Stg.Syn (Native(..))


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

jsonStr :: String -> BS.ByteString
jsonStr s = BS.concat ["\"", encodeUtf8 $ pack s', "\""]
  where
    s' = concatMap escape s
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c = [c]

formatScalar :: Native -> BS.ByteString
formatScalar (NativeNumber n) =
  case floatingOrInteger n of
    Left r -> encodeUtf8 $ pack $ show r
    Right i -> encodeUtf8 $ pack $ show i
formatScalar (NativeSymbol s) = jsonStr s
formatScalar (NativeString s) = jsonStr s
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
