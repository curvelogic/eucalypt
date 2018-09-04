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
import Eucalypt.Stg.Syn
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QM

import Test.Hspec

main :: IO ()
main = hspec spec

calculatesBool :: String -> (Bool -> Bool -> Bool) -> Bool -> Bool -> Property
calculatesBool bif op l r =
  QM.monadicIO $
  calculates
    (appfn_ (Global bif) [Literal $ NativeBool l, Literal $ NativeBool r])
    (returnsNative (NativeBool (l `op` r)))

spec :: Spec
spec =
  describe "boolean globals" $ do
    it "has TRUE" $
      nativeReturn <$>
      test (Atom (Global "TRUE")) `shouldReturn` NativeBool True
    it "has FALSE" $
      nativeReturn <$>
      test (Atom (Global "FALSE")) `shouldReturn` NativeBool False
    it "fails sensibly" $
      test (Atom (Global "NONESUCH")) `shouldThrow`
      (\s -> stgExcError s == UnknownGlobal "NONESUCH")
    context "IF builtin" $ do
      it "selects true for true and doesn't eval false branch" $
        nativeReturn <$>
        test
          (appfn_
             (Global "IF")
             [ Literal $ NativeBool True
             , Literal $ NativeSymbol "foo"
             , Global "BOMB"
             ]) `shouldReturn`
        NativeSymbol "foo"
      it "selects false for false and doesn't eval true branch" $
        nativeReturn <$>
        test
          (appfn_
             (Global "IF")
             [ Literal $ NativeBool False
             , Global "BOMB"
             , Literal $ NativeSymbol "bar"
             ]) `shouldReturn`
        NativeSymbol "bar"
    context "logic builtins" $ do
      it "and" $ property $ calculatesBool "AND" (&&)
      it "or" $ property $ calculatesBool "OR" (||)
      it "and shortcuits" $
        nativeReturn <$>
        test (appfn_ (Global "AND") [Literal $ NativeBool False, Global "BOMB"]) `shouldReturn`
        NativeBool False
      it "or shortcuits" $
        nativeReturn <$>
        test (appfn_ (Global "OR") [Literal $ NativeBool True, Global "BOMB"]) `shouldReturn`
        NativeBool True
