{-|
Module      : Eucalypt.Stg.StgTestUtil
Description : Testing utilities for STG machine tests
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.StgTestUtil where

import Data.Foldable (toList, traverse_)
import Eucalypt.Stg.Compiler
import Eucalypt.Stg.Eval (run)
import Eucalypt.Stg.Event
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Machine
import Eucalypt.Stg.StandardMachine
import qualified Text.PrettyPrint as P
import qualified Test.QuickCheck.Monadic as QM

-- | List of literals
litList_ :: Int -> [Native] -> StgSyn
litList_ envN nats = list_ envN $ map Literal nats

nat :: Integer -> Native
nat n = NativeNumber $ fromInteger n

kv :: String -> Integer -> StgSyn
kv k v =
  letrec_
    [ pc0_ nilConstructor
    , pc_ [Literal $ nat v, Local 0] consConstructor
    , pc_ [Literal $ NativeSymbol k, Local 1] consConstructor
    ]
    (App (Ref (Local 2)) mempty)

block :: [StgSyn] -> StgSyn
block kvs =
  let pcs = map (pc0_ . value_) kvs
      itemCount = length pcs
      itemRefs = [(Local . fromIntegral) n | n <- [0 .. itemCount - 1]]
      l = pc_ itemRefs $ thunkn_ itemCount $ list_ itemCount itemRefs
      bl = pc_ [Local $ fromIntegral itemCount] blockConstructor
      pcs' = pcs ++ [l, bl]
   in letrec_ pcs' (App (Ref (Local (fromIntegral (itemCount + 1)))) mempty)

-- machine state accessors and assertions

-- | Machine is in state with code that returns constructor
returnsConstructor :: Tag -> MachineState -> Bool
returnsConstructor t MachineState {machineCode = (ReturnCon tag _)} = t == tag
returnsConstructor _ _ = False

-- | Extract the native return value from the machine state
nativeReturn :: MachineState -> Native
nativeReturn MachineState {machineCode = (ReturnLit ret)} = ret
nativeReturn ms = error $ "Expected native return, got" ++ show (machineCode ms)

-- | Machine is in state which returns specified native
returnsNative :: Native -> MachineState -> Bool
returnsNative n MachineState {machineCode = (ReturnLit ret)} = ret == n
returnsNative _ _ = False

-- | Machine has logged events specified
emits :: [Event] -> MachineState -> Bool
emits events MachineState {machineDebugEmitLog = logged} = events == logged

-- | Extract the events emitted
emitLog :: MachineState -> [Event]
emitLog = machineDebugEmitLog

-- dump / trace helpers

dumpEnv :: ValVec -> IO ()
dumpEnv (ValVec env)  = traverse_ dumpVal $ toList env
  where
    dumpVal (StgNat n) = putStrLn $ P.render $ prettify n
    dumpVal (StgAddr a) = peek a >>= (putStrLn . P.render . prettify)

dumpEvalEnv :: MachineState -> IO ()
dumpEvalEnv MachineState {machineCode = (Eval _code env)} = dumpEnv env
dumpEvalEnv _ = error "Code is not Eval"

-- machine initialisation and test wrappers

machine :: StgSyn -> IO MachineState
machine = initStandardMachineState

testMachine :: StgSyn -> IO MachineState
testMachine syn = machine syn >>= (\s -> return s{ machineEmit = dumpEmission , machineDebug = True})

tracingMachine :: StgSyn -> IO MachineState
tracingMachine = initDebugMachineState

test :: StgSyn -> IO MachineState
test s = testMachine s >>= run

testTracing :: StgSyn -> IO MachineState
testTracing s = tracingMachine s >>= run

-- quickcheck helpers

calculates :: StgSyn -> (MachineState -> Bool) -> QM.PropertyM IO ()
calculates syn check = QM.run (test syn) >>= (QM.assert . check)

calculatesM :: StgSyn -> (MachineState -> IO Bool) -> QM.PropertyM IO ()
calculatesM syn check = QM.run (test syn) >>= (QM.run . check) >>= QM.assert
