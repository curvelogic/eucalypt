{-|
Module      : Eucalypt.Stg.Event
Description : Spineless tagless G-machine
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Event where

import Eucalypt.Stg.Syn
import Data.ByteString as BS

newtype RenderMetadata = RenderMetadata
  { metaTag :: Maybe String
  } deriving (Show, Eq)

-- | Various events that can be emitted by the machine, including YAML
-- / JSON output rendering and debug tracing.
data Event
  = OutputStreamStart
  | OutputStreamEnd
  | OutputDocumentStart
  | OutputDocumentEnd
  | OutputScalar RenderMetadata !Native
  | OutputNull
  | OutputSequenceStart
  | OutputSequenceEnd
  | OutputMappingStart
  | OutputMappingEnd
  | OutputAlias
  | DebugTrace !BS.ByteString
  deriving (Show, Eq)
