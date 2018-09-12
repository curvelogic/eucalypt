{-|
Module      : Eucalypt.Stg.Globals.BlockSpec
Description : Tests for block globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.BlockSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.Compiler
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.StgTestUtil
import Test.Hspec

main :: IO ()
main = hspec spec

blockA :: StgSyn
blockA =
  letrec_
    [ pc0_ $ thunk_ $ kv "a" 1
    , pc_ [Local 0] $ thunkn_ 1 $ list_ 1 [Local 0] Nothing
    ] $
  appcon_ stgBlock [Local 1]

blockABC :: StgSyn
blockABC =
  letrec_
    [ pc0_ $ thunk_ $ kv "a" 1
    , pc0_ $ thunk_ $ kv "b" 2
    , pc0_ $ thunk_ $ kv "c" 3
    , pc_ (map Local [0 .. 2]) $ thunkn_ 3 $ list_ 3 (map Local [0 .. 2]) Nothing
    ] $
  appcon_ stgBlock [Local 3]

mergeListTwice :: StgSyn -> StgSyn
mergeListTwice b =
  letrec_
    [ pc0_ $ thunk_ b
    , pc_ [Local 0] $ thunkn_ 1 $ appfn_ (Global "MERGE") [Local 0, Local 0]
    ] $
  appfn_ (Global "ELEMENTS") [Local 1]

spec :: Spec
spec =
  describe "__MERGE" $
  context "elements of merge are fully constructed list with evaled keys" $ do
    it "for single element block" $
      (test (mergeListTwice blockA) >>= returnedForcedPairList) `shouldReturn` True
    it "for multi-element block" $
      (test (mergeListTwice blockABC) >>= returnedForcedPairList) `shouldReturn` True
