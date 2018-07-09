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
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Eucalypt.Core.Desugar (desugar)
import Eucalypt.Core.Syn as S
import Eucalypt.Source.Error
import Eucalypt.Syntax.ParseExpr (parseExpression)
import Text.Libyaml
import Text.Regex.PCRE

-- | (When we support aliases - TODO - map aliase to value)
type AnchorMap = Map.Map AnchorName CoreExpr

-- | A CoreExpr together with an AnchorMap containing aliases
data RawExpr =
  RawExpr CoreExpr
          AnchorMap

-- | A scheme for translating YAML into Eucalypt core syntax
class YamlTranslator a where
  handleScalar :: a -> BS.ByteString -> Tag -> Style -> Anchor -> CoreExpr
  handleList :: a -> [CoreExpr] -> CoreExpr
  handleMapping :: a -> [(Text, CoreExpr)] -> CoreExpr

-- | When types are untagged, use rules to resolve
newtype TagResolver =
  TagResolver (String -> Tag)

newtype ActiveTranslator =
  ActiveTranslator TagResolver

newtype InertTranslator =
  InertTranslator TagResolver

-- | Resolve tags as per "10.3.2. Tag Resolution" of the YAML 1.2 spec
coreTagResolve :: String -> Tag
coreTagResolve s
  | s =~ "^[nN]ull$" = NullTag
  | s =~ "^NULL$" = NullTag
  | s =~ "^~$" = NullTag
  | s =~ "^$" = NullTag
  | s =~ "^[tT]rue$" = BoolTag
  | s =~ "^[fF]alse$" = BoolTag
  | s =~ "^TRUE|FALSE$" = BoolTag
  | s =~ "^[-+]?[0-9]+$" = IntTag
  | s =~ "^0o[0-7]+$" = IntTag
  | s =~ "^0x[0-9a-fA-F]+$" = IntTag
  | s =~ "^0x[0-9a-fA-F]+$" = IntTag
  | s =~ "^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$" = FloatTag
  | s =~ "^[-+]?(\\.[0-9]+|[0-9]+(\\.[0-9]*)?)([eE][-+]?[0-9]+)?$" = FloatTag
  | s =~ "^[-+]?(\\.inf|\\.Inf|\\.INF)$" = FloatTag
  | s =~ "^\\.nan|\\.NaN|\\.NAN$" = FloatTag
coreTagResolve _ = StrTag

-- | Translate as inert data - ignore eucalypt tags
instance YamlTranslator InertTranslator where
  handleScalar (InertTranslator (TagResolver resolve)) text tag _ _ =
    case tag' of
      StrTag -> S.str s
      IntTag -> S.int (read s)
      FloatTag -> S.float (read s)
      BoolTag -> S.corebool (read s)
      NullTag -> S.corenull
      _ -> S.str s
    where
      s = (unpack . decodeUtf8) text
      tag' =
        if tag == NoTag
          then resolve s
          else tag
  handleList _ = CoreList
  handleMapping _ pairs = CoreBlock $ CoreList (map kv pairs)
    where
      kv (t, e) = CoreList [CorePrim (S.CoreSymbol (unpack t)), e]

-- | Parse and desugar a eucalypt expression from string
expressionFromString :: String -> CoreExpr
expressionFromString s =
  case parseExpression s "YAML embedding" of
    Left _ -> error $ "Cannot translate expression " ++ s -- TODO
    Right expr -> desugar expr

-- | Active translation scheme
--
-- @!eu@ tag causes expression parse, blocks become let expressions.
instance YamlTranslator ActiveTranslator where
  handleScalar (ActiveTranslator (TagResolver resolve)) text tag _ _ =
    case tag' of
      StrTag -> S.str s
      IntTag -> S.int (read s)
      FloatTag -> S.float (read s)
      BoolTag -> S.corebool (read s)
      NullTag -> S.corenull
      UriTag u ->
        if u == "!eu"
          then expressionFromString s
          else S.str s
      _ -> S.str s
    where
      s = (unpack . decodeUtf8) text
      tag' =
        if tag == NoTag
          then resolve s
          else tag
  handleList _ = CoreList
  handleMapping _ pairs = letexp bindings body
    where
      bindings = map (first unpack) pairs
      body = block [element n (var n) | n <- map (unpack . fst) pairs]

-- | Track a reference to be linked up later
coreAlias :: AnchorName -> CoreExpr
coreAlias = undefined -- TODO

-- | Conduit sink to receive yaml parse events and construct CoreExpr
-- and AnchorMap
sinkExpr ::
     (MonadThrow m, YamlTranslator t)
  => t
  -> ConduitM Event o (WriterT AnchorMap m) CoreExpr
sinkExpr t = start
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
      let scalar = handleScalar t text tag style anchor
       in case anchor of
            Nothing -> return scalar
            Just alias -> tell' anchor scalar >> return (coreAlias alias)
    go (EventSequenceStart anchor) = do
      vals <- goS id
      let val = handleList t vals
      tell' anchor val
    go (EventMappingStart anchor) = do
      pairs <- goM id
      let val = handleMapping t pairs
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
          _ <- tell' anchor $ handleScalar t text tag style anchor
          let k = decodeUtf8 text
          v <- start
          goM (front . ((k, v) :))
        Just e -> throwM $ UnexpectedEvent e

-- | Conduit consumer that delivers a RawExpr from Yaml event stream
sinkRawExpr :: MonadThrow m => ConduitM Event o m RawExpr
sinkRawExpr =
  uncurry RawExpr <$>
  runWriterC (sinkExpr $ InertTranslator (TagResolver coreTagResolve))

-- | Conduit consumer that delivers a RawExpr from Yaml event stream
sinkActiveRawExpr :: MonadThrow m => ConduitM Event o m RawExpr
sinkActiveRawExpr =
  uncurry RawExpr <$>
  runWriterC (sinkExpr $ ActiveTranslator (TagResolver coreTagResolve))

-- | Alter a CoreExpr to reference common values
incorporateAliases :: RawExpr -> CoreExpr
incorporateAliases (RawExpr e _) = e -- TODO: Yaml aliases

-- | Parse inert YAML data into a CoreExpr
parseYamlData :: BS.ByteString -> IO CoreExpr
parseYamlData s = incorporateAliases <$> runConduitRes (decode s .| sinkRawExpr)

-- | Parse eu-annotated active YAML into a CoreExpr
parseYamlExpr :: BS.ByteString -> IO CoreExpr
parseYamlExpr s =
  incorporateAliases <$> runConduitRes (decode s .| sinkActiveRawExpr)
