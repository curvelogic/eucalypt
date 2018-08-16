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

import Eucalypt.Stg.Event
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "RENDER global" $ do
    it "renders native int" $
      emitLog <$>
      testD (appfn_ (Global "RENDER") [Literal $ NativeNumber 1]) `shouldReturn`
      [OutputScalar $ NativeNumber 1]
    it "renders native float" $
      emitLog <$>
      testD (appfn_ (Global "RENDER") [Literal $ NativeNumber 1.9]) `shouldReturn`
      [OutputScalar $ NativeNumber 1.9]
    it "renders native string" $
      emitLog <$>
      testD (appfn_ (Global "RENDER") [Literal $ NativeString "foo"]) `shouldReturn`
      [OutputScalar $ NativeString "foo"]
    it "renders native symbol" $
      emitLog <$>
      testD (appfn_ (Global "RENDER") [Literal $ NativeSymbol "foo"]) `shouldReturn`
      [OutputScalar $ NativeSymbol "foo"]
    it "renders native bool" $
      emitLog <$>
      testD (appfn_ (Global "RENDER") [Literal $ NativeBool True]) `shouldReturn`
      [OutputScalar $ NativeBool True]
    it "renders lists" $
      emitLog <$>
      testD
        (let_
           [pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])]
           (appfn_ (Global "RENDER") [Local 0])) `shouldReturn`
      [ OutputSequenceStart
      , OutputScalar $ NativeSymbol "a"
      , OutputScalar $ NativeSymbol "b"
      , OutputScalar $ NativeSymbol "c"
      , OutputSequenceEnd
      ]
    it "renders blocks" $
      emitLog <$>
      testD
        (let_
           [pc0_ $ thunk_ $ block [kv "a" 1, kv "b" 2]]
           (appfn_ (Global "RENDER") [Local 0])) `shouldReturn`
      [ OutputMappingStart
      , OutputScalar $ NativeSymbol "a"
      , OutputScalar $ NativeNumber 1
      , OutputScalar $ NativeSymbol "b"
      , OutputScalar $ NativeNumber 2
      , OutputMappingEnd
      ]
