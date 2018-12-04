{-|
Module      : Eucalypt.Stg.Globals.NumberSpec
Description : Tests for number globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.NumberSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.StgTestUtil
import Test.Hspec

main :: IO ()
main = hspec spec

parseNumber :: String -> StgSyn
parseNumber text = appfn_ (Global "NUMPARSE") [Literal $ NativeString text]


spec :: Spec
spec =
  describe "__NUMPARSE" $ do
    it "parses integers" $
      (returnsNative (NativeNumber 999) <$> test (parseNumber "999")) `shouldReturn`
      True
    it "parses negative integers" $
      (returnsNative (NativeNumber (-999)) <$> test (parseNumber "-999")) `shouldReturn`
      True
    it "parses floats" $
      (returnsNative (NativeNumber 1.999) <$> test (parseNumber "1.999")) `shouldReturn`
      True
    it "parses negative floats" $
      (returnsNative (NativeNumber (-1.999)) <$> test (parseNumber "-1.999")) `shouldReturn`
      True
    it "parses zero" $
      (returnsNative (NativeNumber 0) <$> test (parseNumber "0")) `shouldReturn`
      True
    it "parses 0.0" $
      (returnsNative (NativeNumber 0) <$> test (parseNumber "0.0")) `shouldReturn`
      True
    it "throws InvalidNumber for bad format" $
      test (parseNumber "blah") `shouldThrow`
      (\s -> stgExcError s == InvalidNumber "blah")
