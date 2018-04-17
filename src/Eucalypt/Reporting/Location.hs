module Eucalypt.Reporting.Location where


data SourcePosition = SourcePosition
  { sourceName :: String
  , sourceColumn :: Int
  , sourceLine :: Int
  }



data SourceSpan = SourceSpan
  { posStart :: SourcePosition
  , posEnd :: SourcePosition
  }
