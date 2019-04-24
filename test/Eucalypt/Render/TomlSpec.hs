{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Render.Toml
Description : Test TOML rendering
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.TomlSpec
  ( main
  , spec
  ) where

import Conduit
import qualified Data.ByteString as BS
import Eucalypt.Stg.Native
import qualified Eucalypt.Stg.Event as E
import Eucalypt.Render.Toml
import Test.Hspec
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)

main :: IO ()
main = hspec spec

test1 :: [E.Event]
test1 =
  [ E.OutputMappingStart (E.RenderMetadata Nothing)
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "a"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeNumber 1234
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "b"
  , E.OutputMappingStart (E.RenderMetadata Nothing)
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "foo"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeString "y"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "bar"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeString "z"
  , E.OutputMappingEnd
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "c"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeNumber 22.22
  , E.OutputMappingEnd
  ]

test2 :: [E.Event]
test2 =
  [E.OutputSequenceStart (E.RenderMetadata Nothing)] <>
  map (E.OutputScalar (E.RenderMetadata Nothing) . NativeNumber . fromInteger) [1 .. 7] <>
  [E.OutputSequenceEnd]

testNull :: [E.Event]
testNull =
  [ E.OutputMappingStart (E.RenderMetadata Nothing)
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "a"
  , E.OutputNull
  , E.OutputMappingEnd
  ]

testUnicode :: [E.Event]
testUnicode =
  [ E.OutputMappingStart (E.RenderMetadata Nothing)
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeSymbol "α"
  , E.OutputScalar (E.RenderMetadata Nothing) $ NativeString "أ"
  , E.OutputMappingEnd
  ]

wrap :: [E.Event] -> [E.Event]
wrap events =
  [E.OutputStreamStart, E.OutputDocumentStart] ++
  events ++ [E.OutputDocumentEnd, E.OutputStreamEnd]

render :: [E.Event] -> IO BS.ByteString
render es = runConduit $ yieldMany (wrap es) .| pipeline

spec :: Spec
spec =
  describe "TOML rendering" $ do
    it "Renders simple TOML snippet" $
      render test1 `shouldReturn` "a = 1234\nc = 22.22\n\n[b]\n  bar = \"z\"\n  foo = \"y\"\n"
    xit "Handles lists" $
      render test2 `shouldReturn` "xxx"
    it "Handles null" $
      render testNull `shouldReturn` ""
    xit "Handle unicode" $
      render testUnicode `shouldReturn` encodeUtf8 (pack "α = \"أ\"\n")
