{-|
Module      : Eucalypt.Stg.GlobalsSpec
Description : Tests for block globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.GlobalsSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.Compiler
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.StgTestUtil
import Test.Hspec

main :: IO ()
main = hspec spec

unforcedKV :: String -> StgSyn -> StgSyn
unforcedKV k v =
  let_
    [ pc0_ $ value_ $ Atom (Literal $ NativeSymbol k)
    , pc0_ $ value_ v
    ] $
  list_ 2 (map Local [0 .. 1]) Nothing

unforcedList :: StgSyn
unforcedList =
  let_
    [ pc0_ $ thunk_ $ unforcedKV "a" (Atom $ Global "BOMB")
    , pc0_ $ thunk_ $ unforcedKV "b" (Atom $ Global "BOMB")
    , pc0_ $ thunk_ $ unforcedKV "c" (Atom $ Global "BOMB")
    ] $
  list_ 3 (map Local [0 .. 2]) Nothing

forceUnevaledPairList :: StgSyn
forceUnevaledPairList =
  let_ [pc0_ $ thunk_ unforcedList] $
  force_ (appfn_ (Global "seqPairList") [Local 0]) (Atom $ Local 1)

spec :: Spec
spec =
  describe "seqPairList" $
  it "forces elements and keys" $
  (test forceUnevaledPairList >>= returnedForcedPairList) `shouldReturn` True
