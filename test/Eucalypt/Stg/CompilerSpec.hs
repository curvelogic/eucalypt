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

import Eucalypt.Core.AnonSyn as C
import Eucalypt.Core.Syn (CoreExpr)
import Eucalypt.Stg.Compiler
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Globals
import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.StgTestUtil
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
        "letrec {2, G[KNIL]} \\ 2 0 -> C|1 E[0] E[1]\n       {1, E[0]} \\ 2 0 -> C|1 E[0] E[1]\n       {0, E[1]} \\ 2 0 -> C|1 E[0] E[1]\n in *E[2]"
  describe "list_" $
    it "preserves metadata" $ do
      let l = list_ 3 [Local 0, Local 1] (Just $ Local 2)
      (P.render . prettify) l `shouldBe`
        "letrec {E[1], G[KNIL]} \\ 2 0 -> C|1 E[0] E[1]\n       {E[0], E[3]}`E[2]` \\ 2 0 -> C|1 E[0] E[1]\n in *E[4]"
  describe "compilation" $ do
    context "compiles primitives" $ do
      it "compiles ints" $
        comp (C.int 2) `shouldBe` Atom (Literal (NativeNumber 2))
      it "compiles strings" $
        comp (C.str "foo") `shouldBe` Atom (Literal (NativeString "foo"))
      it "compiles strings" $
        comp (C.corebool False) `shouldBe` Atom (Literal (NativeBool False))
    context "compiles metadata-annotated primitives" $ do
      it "compiles metadata annotated ints" $
        compile 0 emptyContext (Just (Global "Q")) (C.int 2 :: CoreExpr) `shouldBe`
        appbif_
          (intrinsicIndex "WITHMETA")
          [Global "Q", Literal (NativeNumber 2.0)]
      it "compiles metadata annotated symbols" $
        compile 0 emptyContext (Just (Global "Q")) (C.sym "foo" :: CoreExpr) `shouldBe`
        appbif_
          (intrinsicIndex "WITHMETA")
          [Global "Q", Literal (NativeSymbol "foo")]
    context "handles simple lists" $ do
      it "compiles an empty list" $
        comp (C.corelist []) `shouldBe` Atom (Global "KNIL")
      it "compiles an singleton list" $
        comp (C.corelist [C.int 2]) `shouldBe`
        letrec_
          [pc_ [Literal $ NativeNumber 2, Global "KNIL"] consConstructor]
          (Atom $ Local 0)
    context "handles simple blocks" $ do
      it "compiles an empty block" $
        comp (C.block []) `shouldBe`
        let_ [pc0_ nilConstructor] (appcon_ stgBlock [Local 0])
      it "compiles an simple data block" $
        comp (C.block [C.element "a" $ C.str "a", C.element "b" $ C.str "b"]) `shouldBe`
        asAndBs
    context "handles catenation" $
      it "compiles catenations with internal args" $
      comp (C.app (C.bif "CAT") [C.int 5, C.app (C.bif "ADD") [C.int 1]]) `shouldBe`
      let_
        [ pc0_ $
          thunk_ $ ann_ "" 0 $ appfn_ (Global "ADD") [Literal (NativeNumber 1)]
        ]
        (appfn_ (Global "CAT") [Literal (NativeNumber 5), Local 0])
    context "handles lookup" $
      it "compiles lookup correctly" $
      comp
        (C.corelookup
           (C.block [C.element "a" $ C.str "a", C.element "b" $ C.str "b"])
           "a") `shouldBe`
      let_
        [pc0_ $ thunk_ $ ann_ "" 0 asAndBs]
        (appfn_ (Global "LOOKUP") [Literal $ NativeSymbol "a", Local 0])
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
        [ pc0_ $ lam_ 0 2 (ann_ "k" 0 $ Atom (Local 0))
        , pc0_ $
          lam_
            0
            3
            (ann_ "s" 0 $
             let_
               [ pc_ [Local 1, Local 2] $
                 thunkn_ 2 $ ann_ "" 0 $ appfn_ (Local 0) [Local 1]
               , pc_ [Local 0, Local 2] $
                 thunkn_ 2 $ ann_ "" 0 $ appfn_ (Local 0) [Local 1]
               ]
               (appfn_ (Global "CAT") [Local 3, Local 4]))
        ]
        (let_
           [ pc_ [Local 1, Local 0] $
             thunkn_ 2 $ ann_ "" 0 $ appfn_ (Local 0) [Local 1, Local 1]
           ]
           (appfn_ (Global "CAT") [Literal (NativeNumber 5), Local 2]))
  applySpec

asAndBs :: StgSyn
asAndBs = blk
  where
    a2a =
      ann_ "<item>" 0 $
      list_ 0 [Literal (NativeSymbol "a"), Literal (NativeString "a")] Nothing
    b2b =
      ann_ "<item>" 0 $
      list_ 0 [Literal (NativeSymbol "b"), Literal (NativeString "b")] Nothing
    els =
      ann_ "<content>" 0 $
      let_
        [pc0_ $ thunk_ a2a, pc0_ $ thunk_ b2b]
        (list_ 2 [Local 0, Local 1] Nothing)
    blk = let_ [pc0_ $ thunk_ els] (appcon_ stgBlock [Local 0])


applySpec :: Spec
applySpec =
  describe "compilation of CoreApply" $ do
    it "reads strictness" $
      standardGlobalStrictness "ADD" `shouldBe` [Strict, Strict]
    it "uses cases for strict arguments" $
      comp
        (C.app
           (C.bif "ADD")
           [ C.app (C.bif "SUB") [C.int 3, C.int 2]
           , C.app (C.bif "SUB") [C.int 3, C.int 2]
           ]) `shouldBe`
      force_
        (appfn_ (Global "SUB") [Literal $ nat 3, Literal $ nat 2])
        (force_ (appfn_ (Global "SUB") [Literal $ nat 3, Literal $ nat 2]) $
         appfn_ (Global "ADD") [Local 0, Local 1])
    it "uses lets for non-strict arguments" $
      comp (C.app (C.bif "BLOCK") [C.corelist [C.sym "foo"]]) `shouldBe`
      let_
        [ pc0_ $
          thunk_ $
          ann_ "" 0 $
          letrec_
            [pc_ [Literal $ NativeSymbol "foo", Global "KNIL"] consConstructor]
            (Atom $ Local 0)
        ]
        (appfn_ (Global "BLOCK") [Local 0])
    it "combines cases and lets correctly" $
      comp
        (C.app
           (C.bif "IF")
           [ C.app (C.bif "TRUE") []
           , C.app (C.bif "BOMB") []
           , C.app (C.bif "BOMB") []
           ]) `shouldBe`
      force_
        (appfn_ (Global "TRUE") [])
        (let_
           [ pc0_ $ thunk_ $ ann_ "" 0 $ appfn_ (Global "BOMB") []
           , pc0_ $ thunk_ $ ann_ "" 0 $ appfn_ (Global "BOMB") []
           ] $
         appfn_ (Global "IF") [Local 0, Local 1, Local 2])
