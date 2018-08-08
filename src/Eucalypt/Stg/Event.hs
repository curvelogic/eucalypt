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

-- | Various events that can be emitted by the machine, including YAML
-- / JSON output rendering and debug tracing.
data Event
  = OutputStreamStart
  | OutputStreamEnd
  | OutputDocumentStart
  | OutputDocumentEnd
  | OutputScalar !Native
  | OutputSequenceStart
  | OutputSequenceEnd
  | OutputMappingStart
  | OutputMappingEnd
  | OutputAlias
  | DebugTrace !BS.ByteString
  deriving (Show, Eq)
