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
import Eucalypt.Stg.Syn
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

addTest :: StgSyn
addTest =
  letrec_
    [ PreClosure mempty $ LambdaForm 0 0 False (Atom (Literal (NativeInt 1)))
    , PreClosure mempty $ LambdaForm 0 0 False (Atom (Literal (NativeInt 2)))
    , PreClosure (fromList [Local 0, Local 1]) $
      thunk_ $
      caselit_ (Atom (Local 0)) mempty $
      Just
        (caselit_ (Atom (Local 1)) mempty $ Just (appbif_ 0 [Local 2, Local 3]))
    ] $
  caselit_ (Atom (Local 2)) [(NativeInt 3, Atom (Literal (NativeBool True)))] $
  Just (Atom (Literal (NativeBool False)))


blockSpec :: Spec
blockSpec =
  describe "STG Evaluation" $ do
    let s0 = initMachineState (block [kv "a" 1, kv "b" 2]) mempty
    it "evals block letrec to ReturnCon" $
      (returnsConstructor stgBlock <$> run s0) `shouldReturn` True

    let s1 = initDebugMachineState headOfList mempty
    it "returns lit 2" $
      (returnsNative (NativeInt 2) <$> run s1) `shouldReturn` True

    let s3 = initDebugMachineState addTest mempty
    it "returns true" $
      (returnsNative (NativeBool True) <$> run s3) `shouldReturn` True
