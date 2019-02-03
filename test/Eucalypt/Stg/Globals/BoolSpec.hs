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

import Eucalypt.Stg.Error
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QM

import Test.Hspec

main :: IO ()
main = hspec spec

boolGlobal :: Bool -> Ref
boolGlobal True = Global "TRUE"
boolGlobal False = Global "FALSE"

calculatesBool :: String -> (Bool -> Bool -> Bool) -> Bool -> Bool -> Property
calculatesBool bif op l r =
  QM.monadicIO $
  calculates
    (appfn_ (Global bif) [boolGlobal l, boolGlobal r])
    (returnsConstructor (boolTag (l `op` r)))

spec :: Spec
spec =
  describe "boolean globals" $ do
    it "has TRUE" $
      conReturn <$>
      test (Atom (Global "TRUE")) `shouldReturn` stgTrue
    it "has FALSE" $
      conReturn <$>
      test (Atom (Global "FALSE")) `shouldReturn` stgFalse
    it "fails sensibly" $
      test (Atom (Global "NONESUCH")) `shouldThrow`
      (\s -> stgExcError s == UnknownGlobal "NONESUCH")
    context "IF builtin" $ do
      it "selects true for true and doesn't eval false branch" $
        nativeReturn <$>
        test
          (appfn_
             (Global "IF")
             [ Global "TRUE"
             , Literal $ NativeSymbol "foo"
             , Global "BOMB"
             ]) `shouldReturn`
        NativeSymbol "foo"
      it "selects false for false and doesn't eval true branch" $
        nativeReturn <$>
        test
          (appfn_
             (Global "IF")
             [ Global "FALSE"
             , Global "BOMB"
             , Literal $ NativeSymbol "bar"
             ]) `shouldReturn`
        NativeSymbol "bar"
    context "logic builtins" $ do
      it "and" $ property $ calculatesBool "AND" (&&)
      it "or" $ property $ calculatesBool "OR" (||)
      it "and shortcuits" $
        conReturn <$>
        test (appfn_ (Global "AND") [Global "FALSE", Global "BOMB"]) `shouldReturn`
        stgFalse
      it "or shortcuits" $
        conReturn <$>
        test (appfn_ (Global "OR") [Global "TRUE", Global "BOMB"]) `shouldReturn`
        stgTrue
