{-|
Module      : Eucalypt.Source.YamlSource
Description : Ingest YAML (or JSON) into core syntax
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
Description : This is currently heavily based on Snoyman's Data.Yaml
-}
module Eucalypt.Source.YamlSource where

import Conduit
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadThrow, throwM)
import Control.Monad.Trans.Writer.Strict (WriterT, tell)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Eucalypt.Core.Syn as S
import Eucalypt.Source.Error
import Text.Libyaml



-- | (When we support aliases - TODO - map aliase to value)
type AnchorMap = Map.Map AnchorName CoreExpr



-- | A CoreExpr together with an AnchorMap containing aliases
data RawExpr = RawExpr CoreExpr AnchorMap



-- | Parse a Yaml scalar into a Eucalypt primitive.
--
-- TODO: preserve style as metadata
coreScalar :: BS.ByteString -> Tag -> Style -> Anchor -> CoreExpr
coreScalar text tag _ _ =
  let s = (unpack . decodeUtf8) text
   in CorePrim $
      case tag of
        StrTag -> S.CoreString s
        IntTag -> S.CoreInt (read s)
        FloatTag -> S.CoreFloat (read s)
        BoolTag -> S.CoreBoolean (read s)
        NullTag -> S.CoreNull
        _ -> S.CoreString s



-- | Track a reference to be linked up later
coreAlias :: AnchorName -> CoreExpr
coreAlias = undefined -- TODO



-- | Parse a Yaml list into a Eucalypt CoreList
coreList :: [CoreExpr] -> CoreExpr
coreList = CoreList



-- | Parse a Yaml mapping into a Eucalypt block
coreMapping :: [(Text,CoreExpr)] -> CoreExpr
coreMapping pairs = CoreBlock $ CoreList (map kv pairs)
  where
    kv (t, e) = CoreList [CorePrim (S.CoreSymbol (unpack t)), e]



-- | Conduit sink to receive yaml parse events and construct CoreExpr
-- and AnchorMap
sinkInertExpr :: MonadThrow m => ConduitM Event o (WriterT AnchorMap m) CoreExpr
sinkInertExpr = start
  where
    start = await >>= maybe (throwM UnexpectedEndOfEvents) go
    tell' Nothing val = return val
    tell' (Just name) val = do
      lift $ tell $ Map.singleton name val
      return val
    go EventStreamStart = start
    go EventDocumentStart = start
    go (EventAlias a) = return $ coreAlias a
    go (EventScalar text tag style anchor) =
      let scalar = coreScalar text tag style anchor
       in case anchor of
            Nothing -> return scalar
            Just alias -> tell' anchor scalar >> return (coreAlias alias)
    go (EventSequenceStart anchor) = do
      vals <- goS id
      let val = coreList vals
      tell' anchor val
    go (EventMappingStart anchor) = do
      pairs <- goM id
      let val = coreMapping pairs
      tell' anchor val
    go e = throwM $ UnexpectedEvent e
    goS front = do
      me <- await
      case me of
        Nothing -> throwM UnexpectedEndOfEvents
        Just EventSequenceEnd -> return $ front []
        Just e -> do
          val <- go e
          goS (front . (val :))
    goM front = do
      mk <- await
      case mk of
        Nothing -> throwM UnexpectedEndOfEvents
        Just EventMappingEnd -> return $ front []
        Just (EventScalar text tag style anchor) -> do
          _ <- tell' anchor $ coreScalar text tag style anchor
          let k = decodeUtf8 text
          v <- start
          goM (front . ((k, v) :))
        Just e -> throwM $ UnexpectedEvent e



-- | Conduit consumer that delivers a RawExpr from Yaml event stream
sinkRawExpr :: MonadThrow m => ConduitM Event o m RawExpr
sinkRawExpr = uncurry RawExpr <$> runWriterC sinkInertExpr



-- | Alter a CoreExpr to reference common values
incorporateAliases :: RawExpr -> CoreExpr
incorporateAliases (RawExpr e _) = e -- TODO: Yaml aliases



-- | Parse YAML String into a CoreExpr
parseYamlData :: BS.ByteString -> IO CoreExpr
parseYamlData s = incorporateAliases <$> runConduitRes (decode s .| sinkRawExpr)
