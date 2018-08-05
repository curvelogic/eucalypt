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

import Data.Vector (fromList)
import qualified Data.Vector as Vector
import Eucalypt.Stg.Compiler
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
spec = do
  addressSpec
  blockSpec

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
      machineStack (initMachineState (App (Con 99) mempty) mempty) `shouldBe` mempty
    it "resolves env refs" $
      (do r <- allocate dummyClosure
          s <-
            resolveHeapObject
              (singleton (StgAddr r))
              (initMachineState (App (Con 99) mempty) mempty)
              (Local 0)
          peek s) `shouldReturn`
      dummyClosure



kv :: String -> Integer -> StgSyn
kv k v =
  let kn = NativeString k
      vn = NativeInt v
      pcs =
        [ PreClosure mempty nilConstructor
        , PreClosure (fromList [Literal vn, Local 0]) consConstructor
        , PreClosure (fromList [Literal kn, Local 1]) consConstructor
        ]
   in LetRec (fromList pcs) (App (Ref (Local 2)) mempty)

block :: [StgSyn] -> StgSyn
block kvs =
  let pcs =
        fromList $ map (PreClosure mempty . LambdaForm 0 0 False) kvs
      itemCount = Vector.length pcs
      itemRefs = fromList [(Local . fromIntegral) n | n <- [0 .. itemCount-1]]
      bl = PreClosure itemRefs blockConstructor
      pcs' = pcs `Vector.snoc` bl
   in LetRec pcs' (App (Ref (Local (fromIntegral itemCount))) mempty)

headOfList :: StgSyn
headOfList =
  letrec_
    [ PreClosure mempty head_
    , PreClosure
        mempty
        (LambdaForm 0 0 True (litList_ [NativeInt 1, NativeInt 2]))
    ]
    (App (Ref (Local 0)) $ Vector.singleton (Local 1))

returnsConstructor :: Tag -> MachineState -> Bool
returnsConstructor t MachineState {machineCode = (ReturnCon tag _)} = t == tag
returnsConstructor _ _ = False

returnsNative :: Native -> MachineState -> Bool
returnsNative n MachineState {machineCode = (ReturnLit ret)} = ret == n
returnsNative _ _ = False

blockSpec :: Spec
blockSpec =
  describe "STG Evaluation" $ do
    let s0 = initMachineState (block [kv "a" 1, kv "b" 2]) mempty
    it "evals block letrec to ReturnCon" $
      (returnsConstructor stgBlock <$> run s0) `shouldReturn` True

    let s1 = initDebugMachineState headOfList mempty
    it "returns lit 2" $
      (returnsNative (NativeInt 2) <$> run s1) `shouldReturn` True
