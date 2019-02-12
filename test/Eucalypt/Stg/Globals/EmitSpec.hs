{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.EmitSpec
Description : Tests for eq globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.EmitSpec
  ( main
  , spec
  ) where

import Data.Symbol ()
import Eucalypt.Stg.Event
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "RENDER global" $ do
    it "renders native int" $
      emitLog <$>
      test (appfn_ (gref "RENDER") [V $ NativeNumber 1]) `shouldReturn`
      [OutputScalar (RenderMetadata Nothing) $ NativeNumber 1]
    it "renders native float" $
      emitLog <$>
      test (appfn_ (gref "RENDER") [V $ NativeNumber 1.9]) `shouldReturn`
      [OutputScalar (RenderMetadata Nothing) $ NativeNumber 1.9]
    it "renders native string" $
      emitLog <$>
      test (appfn_ (gref "RENDER") [V $ NativeString "foo"]) `shouldReturn`
      [OutputScalar (RenderMetadata Nothing) $ NativeString "foo"]
    it "renders native symbol" $
      emitLog <$>
      test (appfn_ (gref "RENDER") [V $ NativeSymbol "foo"]) `shouldReturn`
      [OutputScalar (RenderMetadata Nothing) $ NativeSymbol "foo"]
    it "renders native bool" $
      emitLog <$>
      test (appfn_ (gref "RENDER") [gref "TRUE"]) `shouldReturn`
      [OutputTrue]
    it "renders lists" $
      emitLog <$>
      test
        (let_
           [pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])]
           (appfn_ (gref "RENDER") [L 0])) `shouldReturn`
      [ OutputSequenceStart
      , OutputScalar (RenderMetadata Nothing) $ NativeSymbol "a"
      , OutputScalar (RenderMetadata Nothing) $ NativeSymbol "b"
      , OutputScalar (RenderMetadata Nothing) $ NativeSymbol "c"
      , OutputSequenceEnd
      ]
    it "renders blocks" $
      emitLog <$>
      test
        (let_
           [pc0_ $ thunk_ $ block [kv "a" $ nat 1, kv "b" $ nat 2]]
           (appfn_ (gref "RENDER") [L 0])) `shouldReturn`
      [ OutputMappingStart
      , OutputScalar (RenderMetadata Nothing) $ NativeSymbol "a"
      , OutputScalar (RenderMetadata Nothing) $ NativeNumber 1
      , OutputScalar (RenderMetadata Nothing) $ NativeSymbol "b"
      , OutputScalar (RenderMetadata Nothing) $ NativeNumber 2
      , OutputMappingEnd
      ]
    it "ignores lambdas" $
      emitLog <$>
      test
        (let_
           [pc0_ $ thunk_ $ block [kv "a" $ nat 1, kv "b" $ nat 2]]
           (appfn_ (gref "RENDER") [L 0])) `shouldReturn`
      [ OutputMappingStart
      , OutputScalar (RenderMetadata Nothing) $ NativeSymbol "a"
      , OutputScalar (RenderMetadata Nothing) $ NativeNumber 1
      , OutputScalar (RenderMetadata Nothing) $ NativeSymbol "b"
      , OutputScalar (RenderMetadata Nothing) $ NativeNumber 2
      , OutputMappingEnd
      ]
    it "ignores suppressed KVs" $
      emitLog <$>
      test
        (let_
           [pc0_ $ thunk_ $ block [kv "a" $ nat 1, kv_ "b" $ nat 2]]
           (appfn_ (gref "RENDER") [L 0])) `shouldReturn`
      [ OutputMappingStart
      , OutputScalar (RenderMetadata Nothing) $ NativeSymbol "a"
      , OutputScalar (RenderMetadata Nothing) $ NativeNumber 1
      , OutputMappingEnd
      ]
    -- it "can force blocks for metadata" $
    --   (let_
    --      [pc0_ $ value_ $ (Atom $ V $ nat 1)]
    --      (block [kv "a" $  L 0]))
