{-|
Module      : Eucalypt.Stg.Compiler
Description : Tests for Core -> STG compilation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.CompilerSpec (main, spec)
where

import Eucalypt.Core.Syn as C
import Eucalypt.Stg.Compiler
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Test.Hspec
import Text.PrettyPrint as P

main :: IO ()
main = hspec spec

comp :: C.CoreExpr -> StgSyn
comp = compile 0 emptyContext

spec :: Spec
spec = do
  describe "litList_" $
    it "creates list of natives" $ do
      let l = litList_ 0 (map (NativeNumber . fromInteger) [0, 1, 2])
      (P.render . prettify) l `shouldBe`
        "letrec {} \\ 0 0 -> C|0 \n       {2, E[0]} \\ 2 0 -> C|1 E[0] E[1]\n       {1, E[1]} \\ 2 0 -> C|1 E[0] E[1]\n       {0, E[2]} \\ 2 0 -> C|1 E[0] E[1]\n in E[3] "
  describe "compilation" $ do
    context "compiles primitives" $ do
      it "compiles ints" $
        comp (C.int 2) `shouldBe` Atom (Literal (NativeNumber 2))
      it "compiles strings" $
        comp (C.str "foo") `shouldBe` Atom (Literal (NativeString "foo"))
      it "compiles strings" $
        comp (C.corebool False) `shouldBe` Atom (Literal (NativeBool False))
    context "handles simple lists" $ do
      it "compiles an empty list" $
        comp (C.CoreList []) `shouldBe` App (Con 0) mempty
      it "compiles an singleton list" $
        comp (C.CoreList [C.int 2]) `shouldBe`
        let_
          [pc0_ $ box_ (NativeNumber 2)]
          (letrec_
             [pc0_ nilConstructor, pc_ [Local 0, Local 1] consConstructor]
             (appfn_ (Local 2) mempty))
    context "handles simple blocks" $ do
      it "compiles an empty block" $
        comp (C.block []) `shouldBe`
        let_ [pc0_ nilConstructor] (appcon_ stgBlock [Local 0])
      it "compiles an simple data block" $
        comp (C.block [C.element "a" $ C.str "a", C.element "b" $ C.str "b"]) `shouldBe`
        asAndBs


asAndBs :: StgSyn
asAndBs = blk
  where
    a2a =
      let_
        [pc0_ $ box_ (NativeSymbol "a"), pc0_ $ box_ (NativeString "a")]
        (list_ 2 [Local 0, Local 1])
    b2b =
      let_
        [pc0_ $ box_ (NativeSymbol "b"), pc0_ $ box_ (NativeString "b")]
        (list_ 2 [Local 0, Local 1])
    els =
      let_ [pc0_ $ thunk_ a2a, pc0_ $ thunk_ b2b] (list_ 2 [Local 0, Local 1])
    blk = let_ [pc0_ $ thunk_ els] (appcon_ stgBlock [Local 0])
