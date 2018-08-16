{-|
Module      : Eucalypt.Stg.Machine
Description : Tests for STG machine operation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.MachineSpec (main, spec)
where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Test.Hspec

main :: IO ()
main = hspec spec

dummyStg :: LambdaForm
dummyStg = LambdaForm 0 0 False $ App (Con 0) mempty

dummyClosure :: HeapObject
dummyClosure = Closure dummyStg (ValVec mempty)

spec :: Spec
spec = addressSpec

addressSpec :: Spec
addressSpec = do
  describe "Addresses" $
    it "creates, reads, updates" $
    (do r <- allocate BlackHole
        poke r dummyClosure
        peek r) `shouldReturn`
    dummyClosure
  describe "MachineState" $ do
    it "initialises with blank stack" $
      machineStack <$>
      initStandardMachineState (App (Con 99) mempty) `shouldReturn` mempty
    it "resolves env refs" $
      (do r <- allocate dummyClosure
          ms <- initStandardMachineState (App (Con 99) mempty)
          s <- resolveHeapObject (singleton (StgAddr r)) ms (Local 0)
          peek s) `shouldReturn`
      dummyClosure
