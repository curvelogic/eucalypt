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

import qualified Data.Vector as V
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Intrinsics.Common
import Test.QuickCheck
import Test.QuickCheck.Monadic (run, assert, monadicIO)
import Test.Hspec

main :: IO ()
main = hspec spec

-- | If we allocate and return a list of natives, we should be able to
-- read that list back into haskell land:
readsReturns :: [Native] -> Property
readsReturns ns =
  monadicIO $ do
    ms <- run $ initStandardMachineState (Atom (Literal (NativeSymbol "foo")))
    ms' <- run $ returnNatList ms ns
    case ms' of
      MachineState {machineCode = (ReturnCon c (ValVec xs))}
        | c == stgCons -> do
          let (StgNat h) = V.head xs
          let (StgAddr t) = xs V.! 1
          r <- run $ (h :) <$> readNatList ms' t
          assert $ r == ns
        | c == stgNil -> assert $ null ns
      _ -> assert False

spec :: Spec
spec =
  describe "marshalling lists" $
    it "correctly reads returns" $ property readsReturns
