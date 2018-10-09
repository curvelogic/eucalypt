{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Reporting.Code
Description : Formatting code references for output to console
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Reporting.Code (format, formatPoint, formatSingleLine, formatRegion)
where

import qualified Data.ByteString as B
import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Safe (atMay)
import qualified Text.PrettyPrint as P



-- | Format code evidence appropriately for region size
format :: B.ByteString -> Int -> Int -> Int -> Int -> P.Doc
format text startLine startCol endLine endCol =
  if startLine == endLine then
    if startCol == endCol then
      formatPoint text startLine startCol
    else
      formatSingleLine text startLine startCol endCol
  else
    formatRegion text startLine startCol endLine endCol



-- | Prefixes each supplied line with a line numbering margin with
-- numbers from 'from' and an unnumbered top and bottom fringe, each
-- of height 'm'
againstLineNumberedMargin :: Int -> Int -> [P.Doc] -> P.Doc
againstLineNumberedMargin from m ls = P.vcat $ zipWith formLine margin ls
  where
    formLine n l = n P.<+> P.char '|' P.<+> l
    margin = marginPad ++ map fmtLine [from .. to] ++ marginPad
    marginPad = replicate m fringe
    fringe = P.sizedText width (replicate width ' ')
    to = from + length ls - 2 * m - 1
    width = 1 + length (show to)
    fmtLine l =
      let rep = show l
          pad = replicate (width - length rep) ' '
       in P.sizedText width $ pad ++ rep



-- | Read the specified line out of the supplied byte string. (Line
-- indexes are 1-based)
selectLine :: B.ByteString -> Int -> Maybe String
selectLine text n =
  unpack . decodeUtf8 <$>
  atMay (B.splitWith (== fromIntegral (ord '\n')) text) (n - 1)



-- | Format code where the region of interest is a zero-width location
formatPoint :: B.ByteString -> Int -> Int -> P.Doc
formatPoint text line column =
  againstLineNumberedMargin
    line
    2
    [P.empty, P.empty, lineText, pointer1, P.empty]
  where
    lineText = P.text $ fromMaybe "" $ selectLine text line
    pointer1 = P.text $ replicate (column - 1) '-' ++ "^"
    _pointer2 = P.text $ replicate (column - 1) ' ' ++ "|"



-- | Format code where the region of interest is a part of a line
formatSingleLine :: B.ByteString -> Int -> Int -> Int -> P.Doc
formatSingleLine text line from to =
  againstLineNumberedMargin
    line
    2
    [P.empty, P.empty, lineText, underline, P.empty]
  where
    lineText = P.text $ fromMaybe "" $ selectLine text line
    underline = P.text $ replicate (from - 1) ' ' ++ replicate (to - from) '^'



-- | Format code where the region of interest spans many lines
formatRegion :: B.ByteString -> Int -> Int -> Int -> Int -> P.Doc
formatRegion text fromLine _ toLine _ =
  againstLineNumberedMargin fromLine 1 [P.empty, startLineText, P.text "..."] P.$$
  againstLineNumberedMargin toLine 1 [P.text "...", endLineText, P.empty]
  where
    startLineText = P.text $ fromMaybe "" $ selectLine text fromLine
    endLineText = P.text $ fromMaybe "" $ selectLine text toLine
