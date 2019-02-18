{-# LANGUAGE OverloadedStrings #-}
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
import Data.Maybe (isJust)
import Eucalypt.Stg.Address
import Eucalypt.Stg.Eval (run)
import Eucalypt.Stg.Event
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Native
import Eucalypt.Stg.Pretty
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Machine
import Eucalypt.Stg.StandardMachine
import qualified Text.PrettyPrint as P
import Test.QuickCheck (Gen, oneof, arbitrary)
import qualified Test.QuickCheck.Monadic as QM


-- | Construct a list as a STG LetRec
list_ :: Int -> [Ref] -> Maybe Ref -> StgSyn
list_ _ [] Nothing = Atom $ gref "KNIL"
list_ envSz [] (Just ref) =
  let_
    [pc0m_ ref $ value_ (Atom $ gref "KNIL")]
    (Atom $ L $ fromIntegral envSz)
list_ envSz rs metaref = letrec_ pcs (Atom $ L pcn)
  where
    preclose p v m = pcm_ [v, p] m consConstructor
    valrefs = reverse rs
    prevrefs = [gref "KNIL"] <> map (L . fromIntegral) [envSz..]
    metarefs = replicate (length rs - 1) Nothing <> [metaref]
    pcs = zipWith3 preclose prevrefs valrefs metarefs
    pcn = fromIntegral $ envSz + length pcs - 1


-- | List of literals
litList_ :: Int -> [Native] -> StgSyn
litList_ envN nats = list_ envN (map V nats) Nothing

nat :: Integer -> Native
nat n = NativeNumber $ fromInteger n

kv :: String -> Native -> StgSyn
kv k v =
  letrec_
    [ pc0_ nilConstructor
    , pc_ [V v, L 0] consConstructor
    , pc_ [V $ NativeSymbol $ intern k, L 1] consConstructor
    ]
    (Atom $ L 2)

suppressMeta :: StgSyn
suppressMeta = block [kv "export" (NativeSymbol "suppress")]

-- | An export-suppressed key/value pair
kv_ :: String -> Native -> StgSyn
kv_ k v =
  letrec_
    [ pc0_ $ thunk_ suppressMeta
    , pc_ [V v, gref "KNIL"] consConstructor
    , pcm_ [V $ NativeSymbol $ intern k, L 1] (Just $ L 0) consConstructor
    ]
    (Atom $ L 2)

block :: [StgSyn] -> StgSyn
block kvs =
  let pcs = map (pc0_ . value_) kvs
      itemCount = length pcs
      itemRefs = [(L . fromIntegral) n | n <- [0 .. itemCount - 1]]
      l = pc_ itemRefs $ thunkn_ itemCount $ list_ itemCount itemRefs Nothing
      bl = pc_ [L $ fromIntegral itemCount] blockConstructor
      pcs' = pcs ++ [l, bl]
   in letrec_ pcs' (Atom $ L (fromIntegral (itemCount + 1)))

-- machine state accessors and assertions

-- | Extract the native return value from the machine state
conReturn :: MachineState -> Tag
conReturn MachineState {machineCode = (ReturnCon tag _ _)} = tag
conReturn ms = error $ "Expected native return, got" ++ show (machineCode ms)

-- | Machine is in state with code that returns constructor
returnsConstructor :: Tag -> MachineState -> Bool
returnsConstructor t MachineState {machineCode = (ReturnCon tag _ _)} = t == tag
returnsConstructor _ _ = False

-- | Extract the native return value from the machine state
nativeReturn :: MachineState -> Native
nativeReturn MachineState {machineCode = (ReturnLit ret _)} = ret
nativeReturn ms = error $ "Expected native return, got" ++ show (machineCode ms)

-- | Machine is in state which returns specified native
returnsNative :: Native -> MachineState -> Bool
returnsNative n MachineState {machineCode = (ReturnLit ret _)} = ret == n
returnsNative _ _ = False

-- | Machine is in stte which returns native bool true
returnsTrue :: MachineState -> Bool
returnsTrue = returnsConstructor stgTrue

-- | Check that the return is a fully constructed list of pairs with
-- fully evaled native symbol keys
returnedForcedPairList :: MachineState -> IO Bool
returnedForcedPairList ms@MachineState {machineCode = (ReturnCon TagCons xs _)} =
  case toList xs of
    (h:t:_) -> (&&) <$> isKV h <*> isKVList t
    _ -> return False
  where
    isKV x = isJust <$> (scrape ms x :: IO (Maybe (Symbol, StgValue)))
    isKVList x = isJust <$> (scrape ms x :: IO (Maybe [(Symbol, StgValue)]))
returnedForcedPairList MachineState {machineCode = (ReturnCon TagNil _ _)} =
  return True
returnedForcedPairList _ = return False

-- | Machine has logged events specified
emits :: [Event] -> MachineState -> Bool
emits events MachineState {machineDebugEmitLog = logged} = events == logged

-- | Extract the events emitted
emitLog :: MachineState -> [Event]
emitLog = machineDebugEmitLog

-- dump / trace helpers

dumpEnv :: ValVec -> IO ()
dumpEnv env = traverse_ dumpVal $ toList env
  where
    dumpVal (StgNat n _) = putStrLn $ P.render $ prettify n
    dumpVal (StgAddr a) = peek a >>= (putStrLn . P.render . prettify)

dumpEvalEnv :: MachineState -> IO ()
dumpEvalEnv MachineState {machineCode = (Eval _code env)} = dumpEnv env
dumpEvalEnv _ = error "Code is not Eval"

-- machine initialisation and test wrappers

machine :: StgSyn -> IO MachineState
machine = initStandardMachineState

testMachine :: StgSyn -> IO MachineState
testMachine syn = machine syn >>= (\s -> return s{ machineEmitHook = Just dumpEmission , machineDebug = True})

tracingMachine :: StgSyn -> IO MachineState
tracingMachine = initDebugMachineState

test :: StgSyn -> IO MachineState
test s = testMachine s >>= run

testTracing :: StgSyn -> IO MachineState
testTracing s = tracingMachine s >>= run

-- quickcheck helpers

simpleNative :: Gen Native
simpleNative =
    oneof
      [ NativeNumber <$> arbitrary
      , NativeString <$> arbitrary
      , NativeSymbol <$> arbitrary
      ]

calculates :: StgSyn -> (MachineState -> Bool) -> QM.PropertyM IO ()
calculates syn check = QM.run (test syn) >>= (QM.assert . check)

calculatesM :: StgSyn -> (MachineState -> IO Bool) -> QM.PropertyM IO ()
calculatesM syn check = QM.run (test syn) >>= (QM.run . check) >>= QM.assert
