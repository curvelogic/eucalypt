{-|
Module      : Eucalypt.Stg.Globals.BoolSpec
Description : Tests for bool globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.BoolSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Native
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QM

import Test.Hspec

main :: IO ()
main = hspec spec

boolGlobal :: Bool -> Ref
boolGlobal True = gref "TRUE"
boolGlobal False = gref "FALSE"

calculatesBool :: String -> (Bool -> Bool -> Bool) -> Bool -> Bool -> Property
calculatesBool bif op l r =
  QM.monadicIO $
  calculates
    (appfn_ (gref bif) [boolGlobal l, boolGlobal r])
    (returnsConstructor (boolTag (l `op` r)))

spec :: Spec
spec =
  describe "boolean globals" $ do
    it "has TRUE" $
      conReturn <$>
      test (Atom (gref "TRUE")) `shouldReturn` stgTrue
    it "has FALSE" $
      conReturn <$>
      test (Atom (gref "FALSE")) `shouldReturn` stgFalse
    context "IF builtin" $ do
      it "selects true for true and doesn't eval false branch" $
        nativeReturn <$>
        test
          (appfn_
             (gref "IF")
             [ gref "TRUE"
             , V $ NativeSymbol "foo"
             , gref "BOMB"
             ]) `shouldReturn`
        NativeSymbol "foo"
      it "selects false for false and doesn't eval true branch" $
        nativeReturn <$>
        test
          (appfn_
             (gref "IF")
             [ gref "FALSE"
             , gref "BOMB"
             , V $ NativeSymbol "bar"
             ]) `shouldReturn`
        NativeSymbol "bar"
    context "logic builtins" $ do
      it "and" $ property $ calculatesBool "AND" (&&)
      it "or" $ property $ calculatesBool "OR" (||)
      it "and shortcuits" $
        conReturn <$>
        test (appfn_ (gref "AND") [gref "FALSE", gref "BOMB"]) `shouldReturn`
        stgFalse
      it "or shortcuits" $
        conReturn <$>
        test (appfn_ (gref "OR") [gref "TRUE", gref "BOMB"]) `shouldReturn`
        stgTrue
