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
import Eucalypt.Stg.Event
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import qualified Text.PrettyPrint as P

-- | Machine is in state with code that returns constructor
returnsConstructor :: Tag -> MachineState -> Bool
returnsConstructor t MachineState {machineCode = (ReturnCon tag _)} = t == tag
returnsConstructor _ _ = False

-- | Machine is in state which returns specified native
returnsNative :: Native -> MachineState -> Bool
returnsNative n MachineState {machineCode = (ReturnLit ret)} = ret == n
returnsNative _ _ = False

-- | Machine has logged events specified
emits :: [Event] -> MachineState -> Bool
emits events MachineState {machineDebugEmitLog = logged} = events == logged

dumpEnv :: ValVec -> IO ()
dumpEnv (ValVec env)  = traverse_ dumpVal $ toList env
  where
    dumpVal (StgNat n) = putStrLn $ P.render $ prettify n
    dumpVal (StgAddr a) = peek a >>= (putStrLn . P.render . prettify)

dumpEvalEnv :: MachineState -> IO ()
dumpEvalEnv MachineState {machineCode = (Eval _code env)} = dumpEnv env
dumpEvalEnv _ = error "Code is not Eval"
