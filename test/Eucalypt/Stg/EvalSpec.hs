{-|
Module      : Eucalypt.Stg.Eval
Description : Tests for STG evaluation steps
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.EvalSpec (main, spec)
where

import qualified Data.Sequence as Seq
import Eucalypt.Stg.Compiler
import Eucalypt.Stg.Event
import Eucalypt.Stg.Intrinsics
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.StgTestUtil
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  blockSpec
  metaSpec

-- A test which finds the head of a list
headOfList :: StgSyn
headOfList =
  let_
    [ pc0_ $ thunk_ $ appfn_ (Global "HEAD") []
    , pc0_ $ thunk_ (litList_ 0 [nat 1, nat 2])
    ]
    (App (Ref (Local 0)) $ Seq.singleton (Local 1))

-- A test which adds 1 and 2...
addTest :: StgSyn
addTest =
  letrec_
    [ pc0_ $ value_ (Atom (Literal $ nat 1))
    , pc0_ $ value_ (Atom (Literal $ nat 2))
    , pc_ [Local 0, Local 1] $
      valuen_ 2 $
      force_ (Atom (Local 0)) (force_ (Atom (Local 1)) $ add [Local 2, Local 3])
    ] $
  caselit_ (Atom (Local 2)) [(nat 3, Atom (Literal (NativeBool True)))] $
  Just (Atom (Literal (NativeBool False)))
  where
    add = appbif_ $ intrinsicIndex "ADD"

-- | Test sequencing emit actions
renderEmptyMap :: StgSyn
renderEmptyMap = seq_ emitMS emitME
  where
    emitMS = appbif_ (intrinsicIndex "EMIT{") []
    emitME = appbif_ (intrinsicIndex "EMIT}") []


blockSpec :: Spec
blockSpec =
  describe "STG Evaluation" $ do
    it "evals block letrec to ReturnCon" $
      (returnsConstructor stgBlock <$>
       test (block [kv "a" $ nat 1, kv "b" $ nat 2])) `shouldReturn`
      True
    it "returns lit 1" $
      (returnsNative (nat 1) <$> test headOfList) `shouldReturn` True
    it "returns true" $
      (returnsNative (NativeBool True) <$> test addTest) `shouldReturn` True
    it "emits empty map" $
      (emits [OutputMappingStart, OutputMappingEnd] <$> test renderEmptyMap) `shouldReturn`
      True

evalMetadata :: StgSyn
evalMetadata =
  letrec_
    [ pc0_ $ value_ $ Atom (Literal $ NativeNumber 99)
    , pc_ [Local 0] $
      thunkn_ 1 $
      list_
        1
        [Literal $ NativeNumber 0, Literal $ NativeNumber 1]
        (Just $ Local 0)
    ] $
  case_ (Atom $ Local 1) [(stgCons, (3, Atom $ Local 4))]


metaSpec :: Spec
metaSpec =
  describe "metadata" $
  it "propagates into case data branches" $
  returnsNative (nat 99) <$> test evalMetadata `shouldReturn` True
