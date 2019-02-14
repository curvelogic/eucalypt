{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Eucalypt.Stg.Globals.ListSpec
Description : Tests for list globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.ListSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.Error
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Syn
import Eucalypt.Stg.StgTestUtil
import Test.Hspec

main :: IO ()
main = hspec spec

extractsHead :: StgSyn
extractsHead =
  let_
    [ pc0_ $
      thunk_ $ list_ 0 [gref "TRUE", gref "FALSE", gref "FALSE"] Nothing
    ] $
  appfn_ (gref "HEAD") [L 0]

extractsTail :: StgSyn
extractsTail =
  letrec_
    [ pc0_ $
      thunk_ $ list_ 0 [gref "TRUE", gref "FALSE", gref "FALSE"] Nothing
    , pc_ [L 0] $ thunkn_ 1 $ appfn_ (gref "TAIL") [L 0]
    , pc0_ $ thunk_ $ list_ 0 [gref "FALSE", gref "FALSE"] Nothing
    ] $
  appfn_ (gref "EQ") [L 1, L 2]

conses :: StgSyn
conses =
  letrec_
    [ pc0_ $
      thunk_ $
      list_ 0 [V $ nat 1, V $ nat 2, V $ nat 3] Nothing
    , pc_ [L 0] $
      thunkn_ 1 $ appfn_ (gref "CONS") [V $ nat 0, L 0]
    , pc0_ $
      thunk_ $
      list_
        0
        [V $ nat 0, V $ nat 1, V $ nat 2, V $ nat 3]
        Nothing
    ] $
  appfn_ (gref "EQ") [L 1, L 2]

consesWithEmpty :: StgSyn
consesWithEmpty =
  let_
    [ pc0_ $ thunk_ $ appfn_ (gref "CONS") [V $ nat 0, gref "KNIL"]
    , pc0_ $ thunk_ $ list_ 0 [V $ nat 0] Nothing
    ] $
  appfn_ (gref "EQ") [L 0, L 1]

consesWithListValuedCall :: StgSyn
consesWithListValuedCall =
  letrec_
    [ pc0_ $
      thunk_ $
      list_ 0 [V $ nat 1, V $ nat 2, V $ nat 3] Nothing
    , pc_ [L 0] $ thunkn_ 1 $ appfn_ (gref "TAIL") [L 0]
    , pc_ [L 1] $
      thunkn_ 1 $ appfn_ (gref "CONS") [V $ nat 0, L 0]
    , pc0_ $
      thunk_ $
      list_ 0 [V $ nat 0, V $ nat 2, V $ nat 3] Nothing
    ] $
  appfn_ (gref "EQ") [L 2, L 3]

headEmptyList :: StgSyn
headEmptyList = appfn_ (gref "HEAD") [gref "KNIL"]


spec :: Spec
spec =
  describe "List builtins" $ do
    it "extracts head" $ returnsTrue <$> test extractsHead `shouldReturn` True
    it "extracts tail" $ returnsTrue <$> test extractsTail `shouldReturn` True
    it "conses" $ returnsTrue <$> test conses `shouldReturn` True
    it "conses with empty" $
      returnsTrue <$> test consesWithEmpty `shouldReturn` True
    it "conses with list-valued call" $
      returnsTrue <$> test consesWithListValuedCall `shouldReturn` True
    it "throws on head of empty list" $
      test headEmptyList `shouldThrow`
      (\(e :: StgException) -> stgExcError e == Panic "Head of empty list")
