{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Render.Json
Description : Test JSON rendering
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Render.JsonSpec
  ( main
  , spec
  ) where

import Conduit
import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Eucalypt.Stg.Syn
import qualified Eucalypt.Stg.Event as E
import Eucalypt.Render.Json
import Test.Hspec

main :: IO ()
main = hspec spec

test1 :: [E.Event]
test1 =
  [ E.OutputMappingStart
  , E.OutputScalar $ NativeSymbol "a"
  , E.OutputScalar $ NativeNumber 1234
  , E.OutputScalar $ NativeSymbol "b"
  , E.OutputSequenceStart
  , E.OutputScalar $ NativeString "x"
  , E.OutputScalar $ NativeString "y"
  , E.OutputScalar $ NativeString "z"
  , E.OutputSequenceEnd
  , E.OutputMappingEnd
  ]

test2 :: [E.Event]
test2 =
  [E.OutputSequenceStart] <>
  map (E.OutputScalar . NativeNumber . fromInteger) [1 .. 7] <>
  [E.OutputSequenceEnd]

testNull :: [E.Event]
testNull =
  [ E.OutputMappingStart
  , E.OutputScalar $ NativeSymbol "a"
  , E.OutputNull
  , E.OutputMappingEnd
  ]

render :: [E.Event] -> BS.ByteString
render es = runConduitPure $ yieldMany es .| pipeline


spec :: Spec
spec =
  describe "JSON rendering" $ do
    it "Renders simple JSON snippet" $
      ((decode . BL.fromStrict . render) test1 :: Maybe Value) `shouldBe`
      (decode "{\"a\": 1234, \"b\": [\"x\", \"y\", \"z\"]}" :: Maybe Value)
    it "Renders list" $
      ((decode . BL.fromStrict . render) test2 :: Maybe Value) `shouldBe`
      (decode "[1, 2, 3, 4, 5, 6, 7]" :: Maybe Value)
    it "Renders null" $
      ((decode . BL.fromStrict . render) testNull :: Maybe Value) `shouldBe`
      (decode "{ \"a\": null }" :: Maybe Value)
