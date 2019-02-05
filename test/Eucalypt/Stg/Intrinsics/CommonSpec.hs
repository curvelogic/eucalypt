{-|
Module      : Eucalypt.Stg.Intrinsics.CommonSpec
Description : Tests for common intrinsics utilities
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.CommonSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Machine
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.StandardMachine
import Eucalypt.Stg.Intrinsics.Common
import Test.QuickCheck
import Test.QuickCheck.Monadic (run, assert, monadicIO)
import Test.Hspec

main :: IO ()
main = hspec spec

simpleNativeList :: Gen [Native]
simpleNativeList = sized $ \n -> sequence [simpleNative | _ <- [1 .. n]]

-- | If we allocate and return a list of natives, we should be able to
-- read that list back into haskell land:
readsReturns :: [Native] -> Property
readsReturns ns =
  monadicIO $ do
    ms <- run $ initStandardMachineState (Atom (V (NativeSymbol "foo")))
    ms' <- run $ returnNatList ms ns
    case ms' of
      MachineState {machineCode = (ReturnCon TagCons xs _)} -> do
        let (StgNat h _ :< (StgAddr t :< _)) = asSeq xs
        r <- run $ (h :) <$> readNatList ms' t
        assert $ r == ns
      MachineState {machineCode = (ReturnCon TagNil _ _)} -> assert $ null ns
      _ -> assert False

spec :: Spec
spec =
  describe "marshalling lists" $
    it "correctly reads returns" $ property (forAll simpleNativeList readsReturns)
