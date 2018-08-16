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
import Eucalypt.Stg.Machine
import Eucalypt.Stg.StandardMachine
import qualified Text.PrettyPrint as P
import qualified Test.QuickCheck.Monadic as QM

-- | List of literals
litList_ :: Int -> [Native] -> StgSyn
litList_ envN nats = list_ envN $ map Literal nats

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

machineD :: StgSyn -> IO MachineState
machineD = initDebugMachineState

test :: StgSyn -> IO MachineState
test s = machine s >>= run

testD :: StgSyn -> IO MachineState
testD s = machineD s >>= run

-- quickcheck helpers

calculates :: StgSyn -> (MachineState -> Bool) -> QM.PropertyM IO ()
calculates syn check = QM.run (test syn) >>= (QM.assert . check)

calculatesM :: StgSyn -> (MachineState -> IO Bool) -> QM.PropertyM IO ()
calculatesM syn check = QM.run (test syn) >>= (QM.run . check) >>= QM.assert
