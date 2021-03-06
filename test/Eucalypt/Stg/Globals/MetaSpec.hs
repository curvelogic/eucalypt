{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.MetaSpec
Description : Tests for eq globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.MetaSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Native
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

testMeta :: StgSyn
testMeta = block [kv "foo" $ NativeSymbol "bar"]

metaOnBlock :: StgSyn
metaOnBlock =
  letrec_
    [ pc0_ $ thunk_ $ block [kv "a" $ nat 1]
    , pc0_ $ thunk_ testMeta
    , pc_ [L 0, L 1] $ thunkn_ 2 $ appfn_ (gref "WITHMETA") [L 1, L 0]
    , pc_ [L 2] $ thunkn_ 1 $ appfn_ (gref "META") [L 0]
    ]
    (appfn_ (gref "EQ") [L 1, L 3])

metaOnRef :: Int -> Ref -> StgSyn
metaOnRef envSz r =
  letrec_
    [ pc0_ $ thunk_ testMeta
    , pc_ [r, L h] $
      thunkn_ 2 $ appfn_ (gref "WITHMETA") [L 1, L 0]
    , pc_ [L $ h + 1] $ thunkn_ 1 $ appfn_ (gref "META") [L 0]
    ]
    (appfn_ (gref "EQ") [L h, L $ h + 2])
  where h = fromIntegral envSz

metaOnEnvNat :: StgSyn
metaOnEnvNat =
  let_
    [pc0_ $ lam_ 0 1 $ metaOnRef 1 (L 0)]
    (appfn_ (L 0) [V $ NativeNumber 99])

metaOnEnvCon :: StgSyn
metaOnEnvCon =
  let_
    [pc0_ $ thunk_ $ block [], pc0_ $ lam_ 0 1 $ metaOnRef 1 (L 0)]
    (appfn_ (L 1) [L 0])

spec :: Spec
spec =
  describe "META / WITHMETA" $ do
  it "sets and retrieves on block" $
    returnsTrue <$> test metaOnBlock `shouldReturn` True
  it "sets and retrieves on native in env" $
    returnsTrue <$> test metaOnEnvNat `shouldReturn` True
  it "sets and retrieves on block in env" $
    returnsTrue <$> test metaOnEnvCon `shouldReturn` True
