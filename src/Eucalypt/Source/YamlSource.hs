{-# LANGUAGE RecordWildCards #-}
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
import Control.Exception.Safe (catchJust)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Resource (MonadThrow, throwM)
import Control.Monad.Trans.Writer.Strict (WriterT, tell)
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import Eucalypt.Core.Desugar (desugar)
import Eucalypt.Core.SourceMap
import Eucalypt.Core.Syn as S
import Eucalypt.Source.Error
import Eucalypt.Syntax.Error ()
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
  handleScalar ::
       (Monad m, MonadThrow m)
    => a
    -> BS.ByteString
    -> Tag
    -> Style
    -> Anchor
    -> m CoreExpr
  handleList ::
       (Monad m, MonadThrow m)
    => a
    -> [CoreExpr]
    -> Tag
    -> SequenceStyle
    -> Anchor
    -> m CoreExpr

  -- | Called to translate a mapping, with binding names, key
  -- expression and value expressions
  handleMapping ::
       (Monad m, MonadThrow m)
    => a
    -> [(Text, CoreExpr, CoreExpr)]
    -> Tag
    -> MappingStyle
    -> Anchor
    -> m CoreExpr

-- | When types are untagged, use rules to resolve
newtype TagResolver =
  TagResolver (String -> Tag)

newtype ActiveTranslator =
  ActiveTranslator TagResolver

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

-- | Accept any of the tag:yaml.org,2002:bool in the "Core schema"
--
-- NB. tag has already been resolved as boolean so there should be no
-- error result
parseBool :: String -> Bool
parseBool "TRUE" = True
parseBool "True" = True
parseBool "true" = True
parseBool _ = False

-- | Parse and desugar a eucalypt expression from string.
--
--
-- Supports the @!eu@ tag
-- @
-- x: !eu 25 * 23
-- @
expressionFromString :: (Monad m, MonadThrow m) => String -> m CoreExpr
expressionFromString s =
  case parseExpression s "YAML embedding" of
    Left err -> throwM err
    Right expr -> return $ desugar expr

-- | Create metadata expressing a tag.
tagMeta :: String -> CoreExpr
tagMeta tag = anon S.block [anon element "tag" $ anon S.str tag]

-- | Create metadata expressing a tag.
suppressMeta :: CoreExpr
suppressMeta = anon S.block [anon element "export" $ anon S.sym "suppress"]

-- | Place a metadata wrapping around the specified CoreExpr if the
-- tag dictates it
wrapAccordingToTag :: Tag -> CoreExpr -> CoreExpr
wrapAccordingToTag t e =
  case t of
    UriTag "!eu::suppress" -> anon S.withMeta suppressMeta e
    _ -> e

-- | Active translation scheme
--
-- @!eu@ tag causes expression parse
-- @!eu::suppress@ tag on a key causes the key / value to be
--   suppressed from render
-- All mappings become let expressions
instance YamlTranslator ActiveTranslator where
  handleScalar (ActiveTranslator (TagResolver resolve)) text tag _ _ =
    case tag' of
      StrTag -> return $ anon S.str s
      IntTag -> return $ anon S.int (read s)
      FloatTag -> return $ anon S.float (read s)
      BoolTag -> return $ anon S.corebool (parseBool s)
      NullTag -> return $ anon S.corenull
      UriTag "!eu" -> expressionFromString s
      UriTag "!eu::suppress" -> return $ wrapAccordingToTag tag' $ anon S.sym s
      UriTag t -> return $ anon S.withMeta (tagMeta t) $ anon S.str s
      _ -> return $ anon S.str s
    where
      s = (unpack . decodeUtf8) text
      tag' =
        if tag == NoTag
          then resolve s
          else tag
  handleList _ xs tag _s _a = return $ wrapAccordingToTag tag $ anon CoreList xs
  handleMapping _ triples tag _s _a = return $ anon letexp bindings body
    where
      binding (k, _, val) = (unpack k, val)
      bindings = map binding triples
      body = wrapAccordingToTag tag $ anon block $ map bodyItem triples
      bodyItem (k, CoreMeta _ m (CorePrim _ (CoreString n)), _val)
        | m == suppressMeta =
          anon S.withMeta suppressMeta (anon element n $ anon var (unpack k))
      bodyItem (k, CoreMeta _ m (CorePrim _ (CoreSymbol n)), _val)
        | m == suppressMeta =
          anon S.withMeta suppressMeta (anon element n $ anon var (unpack k))
      bodyItem (k, CorePrim _ (CoreString n), _val) =
        anon element n $ anon var (unpack k)
      bodyItem (k, expr, _val) = anon CoreList [expr, anon var (unpack k)]

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
    go (EventScalar text tag style anchor) = do
      scalar <- handleScalar t text tag style anchor
      case anchor of
        Nothing -> return scalar
        Just alias -> tell' anchor scalar >> return (coreAlias alias)
    go (EventSequenceStart tag style anchor) = do
      vals <- goS id
      val <- handleList t vals tag style anchor
      tell' anchor val
    go (EventMappingStart tag style anchor) = do
      pairs <- goM id
      val <- handleMapping t pairs tag style anchor
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
          expr <- handleScalar t text tag style anchor
          _ <- tell' anchor expr
          let k = decodeUtf8 text
          v <- start
          goM (front . ((k, expr, v) :))
        Just e -> throwM $ UnexpectedEvent e

-- | Conduit consumer that delivers a RawExpr from Yaml event stream
sinkActiveRawExpr :: MonadThrow m => ConduitM Event o m RawExpr
sinkActiveRawExpr =
  uncurry RawExpr <$>
  runWriterC (sinkExpr $ ActiveTranslator (TagResolver coreTagResolve))

-- | Alter a CoreExpr to reference common values
incorporateAliases :: RawExpr -> CoreExpr
incorporateAliases (RawExpr e _) = e -- TODO: Yaml aliases

-- | Parse eu-annotated active YAML into a CoreExpr
parseYamlExpr :: String -> BS.ByteString -> IO CoreExpr
parseYamlExpr inputName s =
  catchJust yamlError
  (incorporateAliases <$> runConduitRes (decode s .| sinkActiveRawExpr))
  (throwM . yamlExceptionToDataParseException inputName)
  where
    yamlError :: YamlException -> Maybe YamlException
    yamlError = Just


yamlExceptionToDataParseException :: String -> YamlException -> DataParseException
yamlExceptionToDataParseException inputName (YamlException msg) = YamlError inputName msg
yamlExceptionToDataParseException inputName YamlParseException {..} =
  YamlParseError
    yamlProblem
    yamlContext
    inputName
    (1 + yamlLine yamlProblemMark)
    (1 + yamlColumn yamlProblemMark)
