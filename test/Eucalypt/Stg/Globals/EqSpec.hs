{-# LANGUAGE OverloadedStrings #-}
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

import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Native
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "EQ global" $ do
    it "equates equal strings" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [V $ NativeString "foo", V $ NativeString "foo"]) `shouldReturn`
      stgTrue
    it "equates equal symbols" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [V $ NativeSymbol "foo", V $ NativeSymbol "foo"]) `shouldReturn`
      stgTrue
    it "equates equal ints" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [V $ NativeNumber 9, V $ NativeNumber 9]) `shouldReturn`
      stgTrue
    it "equates equal floats" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [V $ NativeNumber 9.9, V $ NativeNumber 9.9]) `shouldReturn`
      stgTrue
    it "equates equal bools" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [gref "TRUE", gref "TRUE"]) `shouldReturn`
      stgTrue
    it "distinguishes distinct strings" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [V $ NativeString "foo", V $ NativeString "bar"]) `shouldReturn`
      stgFalse
    it "distinguishes distinct symbols" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [V $ NativeSymbol "foo", V $ NativeSymbol "bar"]) `shouldReturn`
      stgFalse
    it "distinguishes distinct ints" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [V $ NativeNumber 10, V $ NativeNumber 9]) `shouldReturn`
      stgFalse
    it "distinguishes distinct floats" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [V $ NativeNumber 10.9, V $ NativeNumber 9.9]) `shouldReturn`
      stgFalse
    it "distinguishes distinct bools" $
      conReturn <$>
      test
        (appfn_
           (gref "EQ")
           [gref "FALSE", gref "TRUE"]) `shouldReturn`
      stgFalse
    context "handles lists" $ do
      it "agrees [:a, :b, :c] = [:a, :b, :c]" $
        conReturn <$>
        test
          (let_
             [ pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])
             , pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])
             ]
             (appfn_ (gref "EQ") [L 0, L 1])) `shouldReturn`
        stgTrue
      it "denies [:a, :b, :c] = [:a, :b]" $
        conReturn <$>
        test
          (let_
             [ pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])
             , pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b"])
             ]
             (appfn_ (gref "EQ") [L 0, L 1])) `shouldReturn`
        stgFalse
      it "denies [:a, :b] = [:a, :b, :c]" $
        conReturn <$>
        test
          (let_
             [ pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b"])
             , pc0_ $ thunk_ $ litList_ 0 (map NativeSymbol ["a", "b", "c"])
             ]
             (appfn_ (gref "EQ") [L 0, L 1])) `shouldReturn`
        stgFalse
