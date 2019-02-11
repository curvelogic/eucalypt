{-# LANGUAGE OverloadedStrings #-}
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

import Data.Symbol ()
import Eucalypt.Core.AnonSyn as C
import Eucalypt.Core.Syn (CoreExpr)
import Eucalypt.Stg.Compiler
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.StgTestUtil
import Eucalypt.Stg.Native
import Eucalypt.Stg.Pretty
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Test.Hspec
import Text.PrettyPrint as P

main :: IO ()
main = hspec spec

comp :: CoreExpr -> StgSyn
comp = compile 0 emptyContext Nothing

spec :: Spec
spec = do
  describe "litList_" $
    it "creates list of natives" $ do
      let l = litList_ 0 (map (NativeNumber . fromInteger) [0, 1, 2])
      (P.render . prettify) l `shouldBe`
        "letrec {2 G[28]} \\ 2 0 -> C|1 E[0] E[1]\n       {1 E[0]} \\ 2 0 -> C|1 E[0] E[1]\n       {0 E[1]} \\ 2 0 -> C|1 E[0] E[1]\n in *E[2]"
  describe "list_" $
    it "preserves metadata" $ do
      let l = list_ 3 [L 0, L 1] (Just $ L 2)
      (P.render . prettify) l `shouldBe`
        "letrec {E[1] G[28]} \\ 2 0 -> C|1 E[0] E[1]\n       {E[0] E[3]}`E[2]` \\ 2 0 -> C|1 E[0] E[1]\n in *E[4]"
  describe "compilation" $ do
    context "compiles primitives" $ do
      it "compiles ints" $
        comp (C.int 2) `shouldBe` Atom (V (NativeNumber 2))
      it "compiles strings" $
        comp (C.str "foo") `shouldBe` Atom (V (NativeString "foo"))
      it "compiles bools" $
        comp (C.corebool False) `shouldBe` Atom (gref "FALSE")
    context "compiles metadata-annotated primitives" $ do
      it "compiles metadata annotated ints" $
        compile 0 emptyContext (Just (gref "KNIL")) (C.int 2 :: CoreExpr) `shouldBe`
        appbif_
          (intrinsicIndex "WITHMETA")
          [gref "KNIL", V (NativeNumber 2.0)]
      it "compiles metadata annotated symbols" $
        compile 0 emptyContext (Just (gref "KNIL")) (C.sym "foo" :: CoreExpr) `shouldBe`
        appbif_
          (intrinsicIndex "WITHMETA")
          [gref "KNIL", V (NativeSymbol "foo")]
    context "handles simple lists" $ do
      it "compiles an empty list" $
        comp (C.corelist []) `shouldBe` Atom (gref "KNIL")
      it "compiles an singleton list" $
        comp (C.corelist [C.int 2]) `shouldBe`
        letrec_
          [pc_ [V $ NativeNumber 2, gref "KNIL"] consConstructor]
          (Atom $ L 0)
    context "handles simple blocks" $ do
      it "compiles an empty block" $
        comp (C.block []) `shouldBe`
        let_ [pc0_ nilConstructor] (appcon_ stgBlock [L 0])
      it "compiles an simple data block" $
        comp (C.block [C.element "a" $ C.str "a", C.element "b" $ C.str "b"]) `shouldBe`
        asAndBs
    context "handles catenation" $
      it "compiles catenations with internal args" $
      comp (C.app (C.bif "CAT") [C.int 5, C.app (C.bif "ADD") [C.int 1]]) `shouldBe`
      let_
        [ pc0_ $
          thunk_ $ ann_ "" 0 $ appfn_ (gref "ADD") [V (NativeNumber 1)]
        ]
        (appfn_ (gref "CAT") [V (NativeNumber 5), L 0])
    context "handles lookup" $
      it "compiles lookup correctly" $
      comp
        (C.corelookup
           (C.block [C.element "a" $ C.str "a", C.element "b" $ C.str "b"])
           "a") `shouldBe`
      let_
        [pc0_ $ thunk_ $ ann_ "" 0 asAndBs]
        (appfn_ (gref "LOOKUP") [V $ NativeSymbol "a", L 0])
    context "manages envsize for subexprs" $
      it "factors both free and bound into envsize for subexprs" $
      comp
        (C.letexp
           [ ("k", C.lam ["x", "y"] (var "x"))
           , ( "s"
             , C.lam
                 ["f", "g", "x"]
                 (C.app
                    (C.bif "CAT")
                    [C.app (var "g") [var "x"], C.app (var "f") [var "x"]]))
           ]
           (C.app (C.bif "CAT") [C.int 5, C.app (var "s") [var "k", var "k"]])) `shouldBe`
      letrec_
        [ pc0_ $ lam_ 0 2 (ann_ "k" 0 $ Atom (L 0))
        , pc0_ $
          lam_
            0
            3
            (ann_ "s" 0 $
             let_
               [ pc_ [L 1, L 2] $
                 thunkn_ 2 $ ann_ "" 0 $ appfn_ (L 0) [L 1]
               , pc_ [L 0, L 2] $
                 thunkn_ 2 $ ann_ "" 0 $ appfn_ (L 0) [L 1]
               ]
               (appfn_ (gref "CAT") [L 3, L 4]))
        ]
        (let_
           [ pc_ [L 1, L 0] $
             thunkn_ 2 $ ann_ "" 0 $ appfn_ (L 0) [L 1, L 1]
           ]
           (appfn_ (gref "CAT") [V (NativeNumber 5), L 2]))
  applySpec

asAndBs :: StgSyn
asAndBs = blk
  where
    a2a =
      ann_ "<item>" 0 $
      list_ 0 [V (NativeSymbol "a"), V (NativeString "a")] Nothing
    b2b =
      ann_ "<item>" 0 $
      list_ 0 [V (NativeSymbol "b"), V (NativeString "b")] Nothing
    els =
      ann_ "<content>" 0 $
      let_
        [pc0_ $ thunk_ a2a, pc0_ $ thunk_ b2b]
        (list_ 2 [L 0, L 1] Nothing)
    blk = let_ [pc0_ $ thunk_ els] (appcon_ stgBlock [L 0])


applySpec :: Spec
applySpec =
  describe "compilation of CoreApply" $ do
    it "reads strictness" $
      globalSignature "ADD" `shouldBe` [Strict, Strict]
    it "uses cases for strict arguments" $
      comp
        (C.app
           (C.bif "ADD")
           [ C.app (C.bif "SUB") [C.int 3, C.int 2]
           , C.app (C.bif "SUB") [C.int 3, C.int 2]
           ]) `shouldBe`
      force_
        (appfn_ (gref "SUB") [V $ nat 3, V $ nat 2])
        (force_ (appfn_ (gref "SUB") [V $ nat 3, V $ nat 2]) $
         appfn_ (gref "ADD") [L 0, L 1])
    it "uses lets for non-strict arguments" $
      comp (C.app (C.bif "BLOCK") [C.corelist [C.sym "foo"]]) `shouldBe`
      let_
        [ pc0_ $
          thunk_ $
          ann_ "" 0 $
          letrec_
            [pc_ [V $ NativeSymbol "foo", gref "KNIL"] consConstructor]
            (Atom $ L 0)
        ]
        (appfn_ (gref "BLOCK") [L 0])
    it "combines cases and lets correctly" $
      comp
        (C.app
           (C.bif "IF")
           [ C.app (C.bif "TRUE") []
           , C.app (C.bif "BOMB") []
           , C.app (C.bif "BOMB") []
           ]) `shouldBe`
      force_
        (appfn_ (gref "TRUE") [])
        (let_
           [ pc0_ $ thunk_ $ ann_ "" 0 $ appfn_ (gref "BOMB") []
           , pc0_ $ thunk_ $ ann_ "" 0 $ appfn_ (gref "BOMB") []
           ] $
         appfn_ (gref "IF") [L 0, L 1, L 2])
