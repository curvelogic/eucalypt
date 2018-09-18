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
    , pc_ [Local 0, Local 1] $ thunkn_ 2 $ appfn_ (Global "WITHMETA") [Local 1, Local 0]
    , pc_ [Local 2] $ thunkn_ 1 $ appfn_ (Global "META") [Local 0]
    ]
    (appfn_ (Global "EQ") [Local 1, Local 3])

metaOnRef :: Int -> Ref -> StgSyn
metaOnRef envSize r =
  letrec_
    [ pc0_ $ thunk_ testMeta
    , pc_ [r, Local h] $
      thunkn_ 2 $ appfn_ (Global "WITHMETA") [Local 1, Local 0]
    , pc_ [Local $ h + 1] $ thunkn_ 1 $ appfn_ (Global "META") [Local 0]
    ]
    (appfn_ (Global "EQ") [Local h, Local $ h + 2])
  where h = fromIntegral envSize

metaOnEnvNat :: StgSyn
metaOnEnvNat =
  let_
    [pc0_ $ lam_ 0 1 $ metaOnRef 1 (Local 0)]
    (appfn_ (Local 0) [Literal $ NativeNumber 99])

metaOnEnvCon :: StgSyn
metaOnEnvCon =
  let_
    [pc0_ $ thunk_ $ block [], pc0_ $ lam_ 0 1 $ metaOnRef 1 (Local 0)]
    (appfn_ (Local 1) [Local 0])

spec :: Spec
spec =
  describe "META / WITHMETA" $ do
  it "sets and retrieves on block" $
    returnsTrue <$> test metaOnBlock `shouldReturn` True
  it "sets and retrieves on native in env" $
    returnsTrue <$> test metaOnEnvNat `shouldReturn` True
  it "sets and retrieves on block in env" $
    returnsTrue <$> test metaOnEnvCon `shouldReturn` True
