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

import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.StgTestUtil
import Test.Hspec

main :: IO ()
main = hspec spec

blockA :: StgSyn
blockA =
  letrec_
    [ pc0_ $ thunk_ $ kv "a" $ nat 1
    , pc_ [L 0] $ thunkn_ 1 $ list_ 1 [L 0] Nothing
    ] $
  appcon_ stgBlock [L 1]

blockABC :: StgSyn
blockABC =
  letrec_
    [ pc0_ $ thunk_ $ kv "a" $ nat 1
    , pc0_ $ thunk_ $ kv "b" $ nat 2
    , pc0_ $ thunk_ $ kv "c" $ nat 3
    , pc_ (map L [0 .. 2]) $ thunkn_ 3 $ list_ 3 (map L [0 .. 2]) Nothing
    ] $
  appcon_ stgBlock [L 3]

mergeListTwice :: StgSyn -> StgSyn
mergeListTwice b =
  letrec_
    [ pc0_ $ thunk_ b
    , pc_ [L 0] $ thunkn_ 1 $ appfn_ (gref "MERGE") [L 0, L 0]
    ] $
  appfn_ (gref "ELEMENTS") [L 1]

spec :: Spec
spec =
  describe "__MERGE" $
  context "elements of merge are fully constructed list with evaled keys" $ do
    it "for single element block" $
      (test (mergeListTwice blockA) >>= returnedForcedPairList) `shouldReturn` True
    it "for multi-element block" $
      (test (mergeListTwice blockABC) >>= returnedForcedPairList) `shouldReturn` True


-- blockSpec :: Spec
-- blockSpec =
--   describe "Block merges" $
--     it "overrides preserving original order" $
--       let b1 = block [element "b" $ int 1, element "a" $ int 5]
--           b2 = block [element "b" $ int (-1)]
--           b3 = block [element "b" $ int (-1), element "a" $ int 5]
--           merged = runRightInterpreter $ euMerge return [b1, b2]
--       in (merged `shouldBe` b3)
