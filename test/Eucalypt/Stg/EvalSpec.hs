{-|
Module      : Eucalypt.Stg.Eval
Description : Tests for STG evaluation steps
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.EvalSpec (main, spec)
where

import Data.Vector (fromList)
import qualified Data.Vector as Vector
import Eucalypt.Stg.Compiler
import Eucalypt.Stg.Eval
import Eucalypt.Stg.Event
import Eucalypt.Stg.Intrinsics
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Machine
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = blockSpec

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

-- A test which finds the head of a list
headOfList :: StgSyn
headOfList =
  let_
    [ PreClosure mempty head_
    , PreClosure
        mempty
        (LambdaForm 0 0 True (litList_ 0 [NativeInt 1, NativeInt 2]))
    ]
    (App (Ref (Local 0)) $ Vector.singleton (Local 1))

-- A test which adds 1 and 2...
addTest :: StgSyn
addTest =
  letrec_
    [ PreClosure mempty $ LambdaForm 0 0 False (Atom (Literal (NativeInt 1)))
    , PreClosure mempty $ LambdaForm 0 0 False (Atom (Literal (NativeInt 2)))
    , PreClosure (fromList [Local 0, Local 1]) $
      thunk_ $
      caselit_ (Atom (Local 0)) mempty $
      Just
        (caselit_ (Atom (Local 1)) mempty $ Just (add [Local 2, Local 3]))
    ] $
  caselit_ (Atom (Local 2)) [(NativeInt 3, Atom (Literal (NativeBool True)))] $
  Just (Atom (Literal (NativeBool False)))
  where
    add = appbif_ $ intrinsicIndex "ADD"

-- | Test sequencing emit actions
renderEmptyMap :: StgSyn
renderEmptyMap = seq_ emitMS emitME
  where
    emitMS = appbif_ (intrinsicIndex "EMIT{") []
    emitME = appbif_ (intrinsicIndex "EMIT}") []
    
-- | A crude rendering function to resolve to NF and pass data
-- structures to emit
_render :: StgSyn
_render =
  letrec_
      -- emptyList
    [ PreClosure mempty $ LambdaForm 0 0 False $ seq_ emitSS emitSE
      -- continueList
    , PreClosure (fromList [continueList]) $
      LambdaForm 1 1 False $
      case_
        (Atom (BoundArg 0))
        [(stgCons, (2, appfn_ (Local 0) [Local 2])), (stgNil, (0, emitSE))]
      -- startList
    , PreClosure (fromList [continueList]) $
      LambdaForm 0 2 False $ seq_ emitSS (appfn_ (Local 0) [BoundArg 1])
      -- wrapBlock
    , PreClosure (fromList [typeSwitch]) $
      LambdaForm 1 1 False $
      seqall_ [emitMS, appfn_ (Local 0) [BoundArg 0], emitME]
      -- typeSwitch
    , PreClosure (fromList [emptyList, continueList, startList, wrapBlock]) $
      LambdaForm 4 1 True $
      case_
        (Atom (BoundArg 0))
        [ (stgBlock, (1, appfn_ (Local 3) [Local 5]))
        , (stgCons, (2, appfn_ (Local 2) [Local 5, Local 6]))
        , (stgNil, (0, appfn_ (Local 0) []))
        ]
    ]
    (Atom (Local 4))
  where
    emptyList = Local 0
    continueList = Local 1
    startList = Local 2
    wrapBlock = Local 3
    typeSwitch = Local 4
    emitMS = appbif_ (intrinsicIndex "EMIT{") []
    emitME = appbif_ (intrinsicIndex "EMIT}") []
    emitSS = appbif_ (intrinsicIndex "EMIT[") []
    emitSE = appbif_ (intrinsicIndex "EMIT]") []
    

returnsConstructor :: Tag -> MachineState -> Bool
returnsConstructor t MachineState {machineCode = (ReturnCon tag _)} = t == tag
returnsConstructor _ _ = False

returnsNative :: Native -> MachineState -> Bool
returnsNative n MachineState {machineCode = (ReturnLit ret)} = ret == n
returnsNative _ _ = False

emits :: [Event] -> MachineState -> Bool
emits events MachineState {machineDebugEmitLog = logged} = events == logged

blockSpec :: Spec
blockSpec =
  describe "STG Evaluation" $ do
    let s0 = initMachineState (block [kv "a" 1, kv "b" 2]) mempty
    it "evals block letrec to ReturnCon" $
      (returnsConstructor stgBlock <$> run s0) `shouldReturn` True

    let s1 = initDebugMachineState headOfList mempty
    it "returns lit 1" $
      (returnsNative (NativeInt 1) <$> run s1) `shouldReturn` True

    let s3 = initDebugMachineState addTest mempty
    it "returns true" $
      (returnsNative (NativeBool True) <$> run s3) `shouldReturn` True

    let s4 = initDebugMachineState renderEmptyMap mempty
    it "emits empty map" $
      (emits [OutputMappingStart, OutputMappingEnd] <$> run s4) `shouldReturn` True
