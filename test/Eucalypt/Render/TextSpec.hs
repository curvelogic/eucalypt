{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Render.Text
Description : Test Text rendering
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.TextSpec
  ( main
  , spec
  ) where
import Conduit
import qualified Data.ByteString as BS
import Data.Text.Encoding (encodeUtf8)
import Eucalypt.Stg.Native
import qualified Eucalypt.Stg.Event as E
import Eucalypt.Render.Text
import Test.Hspec

main :: IO ()
main = hspec spec

test1 :: [E.Event]
test1 =
  [ E.OutputMappingStart
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "a"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeNumber 1234
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "b"
  , E.OutputSequenceStart
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeString "x"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeString "y"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeString "z"
  , E.OutputSequenceEnd
  , E.OutputMappingEnd
  ]

test2 :: [E.Event]
test2 =
  [E.OutputSequenceStart] <>
  map (E.OutputScalar (E.RenderMetadata Nothing) . NativeNumber . fromInteger) [1 .. 7] <>
  [E.OutputSequenceEnd]

testNull :: [E.Event]
testNull =
  [ E.OutputMappingStart
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "a"
  , E.OutputNull
  , E.OutputMappingEnd
  ]

render :: [E.Event] -> IO BS.ByteString
render es = runConduitRes $ yieldMany events .| pipeline
  where
    events =
      [E.OutputStreamStart, E.OutputDocumentStart] <> es <>
      [E.OutputDocumentEnd, E.OutputStreamEnd]

spec :: Spec
spec =
  describe "Text rendering" $ do
    it "Renders simple text snippet" $
      render test1 `shouldReturn`
      encodeUtf8 "a\n1234\nb\nx\ny\nz"
    it "Renders list" $
      render test2 `shouldReturn`
      encodeUtf8 "1\n2\n3\n4\n5\n6\n7"
    it "Ignores null" $
      render testNull `shouldReturn`
      encodeUtf8 "a"
