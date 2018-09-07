{-|
Module      : Eucalypt.Stg.Syn
Description : Tests for STG syntax helpers
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.SynSpec (main, spec)
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import Data.Vector (fromList)
import Eucalypt.Stg.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "isContextualRef" $
    it "classifies" $
    isContextualRef (Global "STR") `shouldBe` False
  describe "refvec" $
    it "generates from locals" $
    locals 2 5 `shouldBe` fromList [Local 2, Local 3, Local 4]
  argRefSpec

argRefSpec :: Spec
argRefSpec =
  describe "argsAt" $ do
    it "leaves global refs intact" $
      argsAt 9 (Global "foo") `shouldBe` Global "foo"
    it "leaves local refs intact" $ argsAt 9 (Local 1) `shouldBe` Local 1
    it "leaves literals intact" $
      argsAt 9 (Literal (NativeString "foo")) `shouldBe`
      Literal (NativeString "foo")
    it "adjusts stack arg refs" $ argsAt 9 (BoundArg 1) `shouldBe` Local 10
    it "adjusts case branches" $
      argsAt
        9
        (BranchTable
           (Map.fromList
              [(1, (1, Atom (BoundArg 0))), (2, (1, Atom (BoundArg 1)))])
           mempty
           (Just (Atom (BoundArg 2)))) `shouldBe`
      BranchTable
        (Map.fromList [(1, (1, Atom (Local 9))), (2, (1, Atom (Local 10)))])
        mempty
        (Just (Atom (Local 11)))
    it "adjusts native case branches" $
      argsAt
        9
        (BranchTable
           mempty
           (HM.fromList
              [ (NativeNumber 0, Atom (BoundArg 0))
              , (NativeNumber 1, Atom (BoundArg 1))
              ])
           (Just (Atom (BoundArg 2)))) `shouldBe`
      BranchTable
        mempty
        (HM.fromList
           [(NativeNumber 0, Atom (Local 9)), (NativeNumber 1, Atom (Local 10))])
        (Just (Atom (Local 11)))
    it "adjusts func refs" $
      argsAt 9 (Ref (BoundArg 0)) `shouldBe` Ref (Local 9)
    it "adjusts preclosures" $
      argsAt 9 (pc_ [BoundArg 1, Local 0] (lam_ 0 1 (Atom (BoundArg 0)))) `shouldBe`
      pc_ [Local 10, Local 0] (lam_ 0 1 (Atom (BoundArg 0)))
    it "adjusts StgSyn letrecs" $
      argsAt
        9
        (letrec_
           [ pc_ [BoundArg 0] (LambdaForm 1 0 False (Atom (Local 0)))
           , pc_ [Local 1] (LambdaForm 0 1 False (Atom (BoundArg 0)))
           ]
           (Atom (BoundArg 0))) `shouldBe`
      letrec_
        [ pc_ [Local 9] (LambdaForm 1 0 False (Atom (Local 0)))
        , pc_ [Local 1] (LambdaForm 0 1 False (Atom (BoundArg 0)))
        ]
        (Atom (Local 9))
