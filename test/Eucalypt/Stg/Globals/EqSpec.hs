{-|
Module      : Eucalypt.Stg.Globals.EqSpec
Description : Tests for eq globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.EqSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "EQ global" $ do
    it "equates equal strings" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeString "foo", Literal $ NativeString "foo"]) `shouldReturn`
      NativeBool True
    it "equates equal symbols" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeSymbol "foo", Literal $ NativeSymbol "foo"]) `shouldReturn`
      NativeBool True
    it "equates equal ints" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeNumber 9, Literal $ NativeNumber 9]) `shouldReturn`
      NativeBool True
    it "equates equal floats" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeNumber 9.9, Literal $ NativeNumber 9.9]) `shouldReturn`
      NativeBool True
    it "equates equal bools" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeBool True, Literal $ NativeBool True]) `shouldReturn`
      NativeBool True
    it "distinguishes distinct strings" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeString "foo", Literal $ NativeString "bar"]) `shouldReturn`
      NativeBool False
    it "distinguishes distinct symbols" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeSymbol "foo", Literal $ NativeSymbol "bar"]) `shouldReturn`
      NativeBool False
    it "distinguishes distinct ints" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeNumber 10, Literal $ NativeNumber 9]) `shouldReturn`
      NativeBool False
    it "distinguishes distinct floats" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeNumber 10.9, Literal $ NativeNumber 9.9]) `shouldReturn`
      NativeBool False
    it "distinguishes distinct bools" $
      nativeReturn <$>
      test
        (appfn_
           (Global "EQ")
           [Literal $ NativeBool False, Literal $ NativeBool True]) `shouldReturn`
      NativeBool False
    context "handles lists" $ do
      it "agrees [:a, :b, :c] = [:a, :b, :c]" $
        nativeReturn <$>
        test
          (let_
             [ pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])
             , pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])
             ]
             (appfn_ (Global "EQ") [Local 0, Local 1])) `shouldReturn`
        NativeBool True
      it "denies [:a, :b, :c] = [:a, :b]" $
        nativeReturn <$>
        test
          (let_
             [ pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])
             , pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b"])
             ]
             (appfn_ (Global "EQ") [Local 0, Local 1])) `shouldReturn`
        NativeBool False
      it "denies [:a, :b] = [:a, :b, :c]" $
        nativeReturn <$>
        test
          (let_
             [ pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b"])
             , pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])
             ]
             (appfn_ (Global "EQ") [Local 0, Local 1])) `shouldReturn`
        NativeBool False
