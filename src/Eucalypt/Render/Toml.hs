{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Eucalypt.Render.Toml
Description : TOML renderer for Eucalypt
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.Toml
  where

import Conduit (ConduitT, Void, mapM_C)
import Control.Exception.Safe (MonadThrow, throwM)
import Data.Conduit.Lift (execStateC)
import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))
import Data.Scientific
import Data.Symbol
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Eucalypt.Render.Error
import qualified Eucalypt.Stg.Event as E
import Eucalypt.Stg.Native
import Toml.PrefixTree
import Toml.Printer (pretty)
import Toml.Type.AnyValue
import Toml.Type.TOML
import qualified Toml.Type.Value as V



-- | Work in progress elements as we work through building the TOML
data Wip
  = Mapping TOML
  | Entry Key
  | Sequence [TOML] [AnyValue]
  deriving (Show)




-- | State for accumulating a TOML model from render events
data TOMLFormatState = TOMLFormatState
  { stack :: [Wip]
  , result :: Maybe TOML
  }
  deriving (Show)




-- | Start off with a blank top level TOML
initState :: TOMLFormatState
initState = TOMLFormatState mempty mempty



-- | Called when a scalar is encountered to incorporate into current
-- state
acceptScalar :: (MonadThrow m, MonadState TOMLFormatState m) => AnyValue -> m ()
acceptScalar v = do
  newStack <- gets stack >>= incorporateScalar v
  modify $ \s -> s {stack = newStack}



-- | Incorporate a single value into a stack
incorporateScalar :: (MonadThrow m) => AnyValue -> [Wip] -> m [Wip]
incorporateScalar v (Entry k:rest) = incorporateKeyScalarPair k v rest
incorporateScalar v (Sequence ts vs:rest) = return $ Sequence ts (v : vs) : rest
incorporateScalar (AnyValue val) st@(Mapping _:_) =
  key >>= \k -> return $ Entry k : st
  where
    key =
      case matchText val of
        Right t -> return $ Key (Piece t :| [])
        Left _ -> throwM $ Unrenderable "toml" NonTextualKey
incorporateScalar _ _ = error "Invalid state while incorporating scalar"



-- | Incorporate a scalar value at a key into a stack
incorporateKeyScalarPair :: (MonadThrow m) => Key -> AnyValue -> [Wip] -> m [Wip]
incorporateKeyScalarPair k v (Mapping m:rest) =
  return $ Mapping (insertKeyAnyVal k v m) : rest
incorporateKeyScalarPair k _ st =
  error $
  "Invalid state while incorporating key / scalar " ++ show k ++ " " ++ show st



-- | Pop the sequence or table we've been working on off the top of
-- the stack and incorporate into the element beneath
pop :: (MonadThrow m, MonadState TOMLFormatState m) => m ()
pop =
  gets stack >>= \case
    [Mapping x] -> put $ TOMLFormatState {stack = [], result = Just x}
    [Sequence ts vs] -> do
      toml <- arraysToTOML ts vs
      put $ TOMLFormatState {stack = [], result = Just toml}
    x:xs -> do
      newStack <- incorporateValue x xs
      modify $ \s -> s {stack = newStack}
    [] -> error "Empty stack while popping collection value."



-- | Ensure that all array elements are either values or tables and
-- set as an array value or table array value.
arraysToTOML :: (MonadThrow m) => [TOML] -> [AnyValue] -> m TOML
arraysToTOML ts vs =
  if not (null ts) && not (null vs)
    then throwM $ Unrenderable "toml" HeterogeneousArray
    else toArrayValue vs >>= \a ->
           return $
           insertTableArrays (Key (Piece "tables" :| [])) (NE.fromList ts) $
           insertKeyAnyVal "value" a mempty



-- | Convert to array or throw exception.
--
-- TOML arrays must be homogeneous.
toArrayValue :: (MonadThrow m) => [AnyValue] -> m AnyValue
toArrayValue vs = case toMArray $ reverse vs of
  Left _ -> throwM $ Unrenderable "toml" HeterogeneousArray
  Right v -> return $ AnyValue v



-- | Pop helper - incorporate single value into stack
incorporateValue :: (MonadThrow m) => Wip -> [Wip] -> m [Wip]
incorporateValue v (Entry k:ts) = incorporateKeyValuePair k v ts
incorporateValue (Mapping m) (Sequence ts vs:rest) = return $ Sequence (m : ts) vs : rest
incorporateValue (Sequence _ vs) rest =
  toArrayValue vs >>= \a -> incorporateScalar a rest
incorporateValue v st =
  error $
  "Invalid state while incorporating value. Value: " ++
  show v ++ " Stack: " ++ show st



-- | Pop helper - incorporate key value pair into stack
incorporateKeyValuePair :: (MonadThrow m) => Key -> Wip -> [Wip] -> m [Wip]
incorporateKeyValuePair k (Mapping v) (Mapping m:rest) =
  return $ Mapping (insertTable k v m) : rest
incorporateKeyValuePair k (Sequence ts vs) (Mapping m:rest) = do
  a <- toArrayValue vs
  let toml = insertTableArrays k (NE.fromList ts) (insertKeyAnyVal k a m)
  return $ Mapping toml : rest
incorporateKeyValuePair k v st =
  error $
  "Invalid state while incorporating key / value. Key: " ++
  show k ++ " Value: " ++ show v ++ " Stack: " ++ show st



-- | Clear out any pending key from the top of the stack
clearKey :: MonadState TOMLFormatState m => m ()
clearKey = gets stack >>= \case
  (Entry _:rest) -> modify $ \s -> s { stack = rest }
  _ -> return ()



-- | Push a new work in progress onto the format stack
push :: MonadState TOMLFormatState m => Wip -> m ()
push wip = do
  st <- gets stack
  modify (\s -> s {stack = wip : st})



-- | Inspect the work in progress on the top of the format stack
top :: MonadState TOMLFormatState m => m Wip
top = gets (Prelude.head . stack)



-- | State operator for encountering a render event
encounterEvent :: (MonadThrow m, MonadState TOMLFormatState m) => E.Event -> m ()
encounterEvent (E.OutputScalar _rm n) = acceptScalar (convert n)
encounterEvent E.OutputNull = clearKey
encounterEvent E.OutputTrue = acceptScalar $ AnyValue $ V.Bool True
encounterEvent E.OutputFalse = acceptScalar $ AnyValue $ V.Bool False
encounterEvent E.OutputMappingStart {} = push $ Mapping mempty
encounterEvent E.OutputMappingEnd {} = pop
encounterEvent E.OutputSequenceStart {} = push $ Sequence mempty mempty
encounterEvent E.OutputSequenceEnd {} = pop
encounterEvent E.OutputStreamStart {} = return ()
encounterEvent E.OutputStreamEnd {} = return ()
encounterEvent E.OutputDocumentStart {} = return ()
encounterEvent E.OutputDocumentEnd {} = return ()
encounterEvent _ = undefined



-- | Convert a native into a TOML value
convert :: Native -> AnyValue
convert (NativeNumber sci) = case floatingOrInteger sci of
  (Left f) -> AnyValue $ V.Double f
  (Right n) -> AnyValue $ V.Integer $ fromIntegral (n :: Integer)
convert (NativeString s) = AnyValue $ V.Text (pack s)
convert (NativeSymbol s) = AnyValue $ V.Text (pack $ unintern s)
convert (NativeDynamic _) = AnyValue $ V.Text "**#DYN**"



-- | Ensure the format state is complete and serialise as utf8
serialise :: TOMLFormatState -> BS.ByteString
serialise TOMLFormatState { result = Just t } = encodeUtf8 $ pretty t
serialise _ = error "Invalid terminal TOML format state"



-- | The conduit pipeline for rendering events to TOML
pipeline :: (MonadThrow m) => ConduitT E.Event Void m BS.ByteString
pipeline = serialise <$> execStateC initState (mapM_C encounterEvent)
