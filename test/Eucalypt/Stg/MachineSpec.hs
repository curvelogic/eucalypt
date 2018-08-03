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

import Control.Monad.Loops (concatM)
import Data.Vector (singleton, fromList)
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
dummyClosure = Closure dummyStg (Env mempty)

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
              (Env (singleton (StackAddr r)))
              (initMachineState (App (Con 99) mempty) mempty)
              (LocalEnv 0)
          peek s) `shouldReturn`
      dummyClosure



kv :: String -> Integer -> StgSyn
kv k v =
  let kn = NativeString k
      vn = NativeInt v
      pcs =
        [ PreClosure mempty nilConstructor
        , PreClosure (fromList [Literal vn, LocalEnv 0]) consConstructor
        , PreClosure (fromList [Literal kn, LocalEnv 1]) consConstructor
        ]
   in LetRec (fromList pcs) (App (Ref (LocalEnv 2)) mempty)

block :: [StgSyn] -> StgSyn
block kvs =
  let pcs =
        fromList $ map (PreClosure mempty . LambdaForm 0 0 False) kvs
      itemCount = Vector.length pcs
      itemRefs = fromList [(LocalEnv . fromIntegral) n | n <- [0 .. itemCount-1]]
      bl = PreClosure itemRefs blockConstructor
      pcs' = pcs `Vector.snoc` bl
   in LetRec pcs' (App (Ref (LocalEnv (fromIntegral itemCount))) mempty)


stepped :: Int -> IO MachineState
stepped n =
  concatM
    (replicate n step)
    (initMachineState (block [kv "a" 1, kv "b" 2]) mempty)

exposeList :: StgSyn
exposeList = case_ (block [kv "a" 1, kv "b" 2]) [(stgBlock, (1, app))]
  where
    app = App (Intrinsic 0) $ Vector.singleton (LocalEnv 0)

headOfList :: StgSyn
headOfList =
  letrec_
    [ (PreClosure mempty head_)
    , (PreClosure
         mempty
         (LambdaForm 0 0 True (litList_ [NativeInt 1, NativeInt 2])))
    ]
    (App (Ref (LocalEnv 0)) mempty)

isEval :: Code -> Bool
isEval (Eval _ _) = True
isEval _ = False

isEnter :: Code -> Bool
isEnter (Enter _) = True
isEnter _ = False

isReturnCon :: Code -> Bool
isReturnCon (ReturnCon _ _) = True
isReturnCon _ = False

-- isTerminate :: Code -> Bool
-- isTerminate Terminate = True
-- isTerminate _ = False

blockSpec :: Spec
blockSpec = do
  describe "block evaluation to WHNF" $ do

    let stepped1 = stepped 1
    context "step 1" $
      it "evals an app" $
      (isEval . machineCode <$> stepped1) `shouldReturn` True

    let stepped2 = stepped 2
    context "step 2" $ do
      it "transitions to enter" $
        (isEnter . machineCode <$> stepped2) `shouldReturn` True
      it "prepares empty stack for thunk call" $
        (Vector.null . machineStack <$> stepped2) `shouldReturn` True

    let stepped3 = stepped 3
    context "step 3" $
      it "transitions to eval" $
        (isEval . machineCode <$> stepped3) `shouldReturn` True

    let stepped4 = stepped 4
    context "step 4" $
      it "transitions to return con" $
        (isReturnCon . machineCode <$> stepped4) `shouldReturn` True

  describe "runs to termination" $ do
    let s0 = initMachineState (block [kv "a" 1, kv "b" 2]) mempty
    it "runs" $
      (machineCounter <$> run s0) `shouldReturn` 5

  describe "expose list withing block" $ do
    let s0 = initMachineState exposeList mempty
    it "runs to completion" $
      (machineCounter <$> run s0) `shouldReturn` 7

  describe "calculate head of list" $ do
    let s0 = initMachineState headOfList mempty
    xit "runs to completion" $
      (machineCounter <$> run s0) `shouldReturn` 7
