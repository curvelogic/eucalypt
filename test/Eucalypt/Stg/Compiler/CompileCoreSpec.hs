{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Compiler.CompileCore
Description : Tests for Core -> STG compilation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.Compiler.CompileCoreSpec (main, spec)
where

import Eucalypt.Core.AnonSyn as C
import Eucalypt.Core.Syn (CoreExpr)
import Eucalypt.Stg.Compiler.Context
import Eucalypt.Stg.Compiler.CompileCore
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Test.Hspec


simpleLet :: CoreExpr
simpleLet = letexp [("x", int 9), ("y", var "x")] $ var "x"

simpleLetSyn :: StgSyn
simpleLetSyn =
  letrec_
    [ pc0_ $ value_ $ Atom (V $ NativeNumber 9)
    , pc_ [L 0] $ valuen_ 1 $ Atom $ L 0
    ]
    (Atom $ L 0)

nestedLet :: CoreExpr
nestedLet =
  let sub = letexp [("x", int 9), ("y", var "x")] $ var "x"
   in letexp
        [ ("top", int 99)
        , ("sub", sub)
        , ("g", corelookup (var "sub") "y")
        ] $
      var "g"

nestedLetSyn :: StgSyn
nestedLetSyn =
  letrec_
    [ pc0_ $ value_ $ Atom (V $ NativeNumber 99)
    , pc0_ $ value_ $ Atom (V $ NativeNumber 9)
    , pc_ [L 1] $ valuen_ 1 $ Atom $ L 0
    , pc_ [L 1] $ thunkn_ 1 $ appfn_ (gref "LOOKUP") [V $ NativeSymbol "y", L 0]
    ]
    (Atom $ L 3)

simpleStrictApply :: CoreExpr
simpleStrictApply =
  letexp
    [ ("i", lam ["x"] $ var "x")
    , ("z", app (bif "ADD") [app (var "i") [sym "x"], app (var "i") [sym "y"]])
    ] $
  var "z"

simpleStrictApplySyn :: StgSyn
simpleStrictApplySyn =
  letrec_
    [ pc0_ $ lam_ 0 1 $ ann_ "i" 0 $ Atom $ L 0
    , pc_ [L 0] $
      thunkn_ 1 $
      force_ (appfn_ (L 0) [V $ NativeSymbol "x"]) $
      force_ (appfn_ (L 0) [V $ NativeSymbol "y"]) $
      appfn_ (gref "ADD") [L 1, L 2]
    ]
    (Atom $ L 1)

simpleNonStrictApply :: CoreExpr
simpleNonStrictApply =
  letexp
    [ ("i", lam ["x"] $ var "x")
    , ("plus", lam ["x", "y"] $ app (bif "ADD") [var "x", var "y"])
    , ("z", app (var "plus") [app (var "i") [sym "x"], app (var "i") [sym "y"]])
    ] $
  var "z"

simpleNonStrictApplySyn :: StgSyn
simpleNonStrictApplySyn =
  letrec_
    [ pc0_ $ lam_ 0 1 $ ann_ "i" 0 $ Atom $ L 0
    , pc0_ $
      lam_ 0 2 $
      ann_ "plus" 0 $
      force_ (Atom $ L 0) $ force_ (Atom $ L 1) $ appfn_ (gref "ADD") [L 2, L 3]
    , pc_ [L 0] $ thunkn_ 1 $ appfn_ (L 0) [V $ NativeSymbol "x"]
    , pc_ [L 0] $ thunkn_ 1 $ appfn_ (L 0) [V $ NativeSymbol "y"]
    , pc_ [L 1, L 2, L 3] $ thunkn_ 3 $ appfn_ (L 0) [L 1, L 2]
    ]
    (Atom $ L 4)

simpleMixedApply :: CoreExpr
simpleMixedApply =
  letexp
    [ ("i", lam ["x"] $ var "x")
    , ( "z"
      , app
          (bif "IF")
          [ app (var "i") [sym "c"]
          , app (bif "PANIC") [sym "t"]
          , app (bif "PANIC") [sym "f"]
          ])
    ] $
  var "z"

simpleMixedApplySyn :: StgSyn
simpleMixedApplySyn =
  letrec_
    [ pc0_ $ lam_ 0 1 $ ann_ "i" 0 $ Atom $ L 0
    , pc0_ $
      thunk_ $
      force_ (Atom $ V $ NativeSymbol "t") $ appfn_ (gref "PANIC") [L 0]
    , pc0_ $
      thunk_ $
      force_ (Atom $ V $ NativeSymbol "f") $ appfn_ (gref "PANIC") [L 0]
    , pc_ [L 1, L 2, L 0] $
      thunkn_ 3 $
      force_ (appfn_ (L 2) [V $ NativeSymbol "c"]) $
      appfn_ (gref "IF") [L 3, L 0, L 1]
    ]
    (Atom $ L 3)

main :: IO ()
main = hspec spec

comp :: CoreExpr -> StgSyn
comp e = compile emptyContext Nothing e 0

spec :: Spec
spec =
  describe "compiling bindings" $ do
    it "explicitly shared natives appear as bindings" $
      comp simpleLet `shouldBe` simpleLetSyn
    it "subordinate lets can be subsumed" $
      comp nestedLet `shouldBe` nestedLetSyn
    it "correctly embed-compiles applies with strict args" $
      comp simpleStrictApply `shouldBe` simpleStrictApplySyn
    it "correctly embed-compiles applies with non-strict args" $
      comp simpleNonStrictApply `shouldBe` simpleNonStrictApplySyn
    it "correctly embed-compiles mixed strictness applies" $
      comp simpleMixedApply `shouldBe` simpleMixedApplySyn


-- From old compiler
-- spec :: Spec
-- spec = do
--   describe "litList_" $
--     it "creates list of natives" $ do
--       let l = litList_ 0 (map (NativeNumber . fromInteger) [0, 1, 2])
--       (P.render . prettify) l `shouldBe`
--         "letrec {2 G[28]} \\ 2 0 -> C|1 E[0] E[1]\n       {1 E[0]} \\ 2 0 -> C|1 E[0] E[1]\n       {0 E[1]} \\ 2 0 -> C|1 E[0] E[1]\n in *E[2]"
--   describe "list_" $
--     it "preserves metadata" $ do
--       let l = list_ 3 [L 0, L 1] (Just $ L 2)
--       (P.render . prettify) l `shouldBe`
--         "letrec {E[1] G[28]} \\ 2 0 -> C|1 E[0] E[1]\n       {E[0] E[3]}`E[2]` \\ 2 0 -> C|1 E[0] E[1]\n in *E[4]"
--   describe "compilation" $ do
--     context "compiles primitives" $ do
--       it "compiles ints" $
--         comp (C.int 2) `shouldBe` Atom (V (NativeNumber 2))
--       it "compiles strings" $
--         comp (C.str "foo") `shouldBe` Atom (V (NativeString "foo"))
--       it "compiles bools" $
--         comp (C.corebool False) `shouldBe` Atom (gref "FALSE")
--     context "compiles metadata-annotated primitives" $ do
--       it "compiles metadata annotated ints" $
--         compile 0 emptyContext (Just (gref "KNIL")) (C.int 2 :: CoreExpr) `shouldBe`
--         appbif_
--           (intrinsicIndex "WITHMETA")
--           [gref "KNIL", V (NativeNumber 2.0)]
--       it "compiles metadata annotated symbols" $
--         compile 0 emptyContext (Just (gref "KNIL")) (C.sym "foo" :: CoreExpr) `shouldBe`
--         appbif_
--           (intrinsicIndex "WITHMETA")
--           [gref "KNIL", V (NativeSymbol "foo")]
--     context "handles simple lists" $ do
--       it "compiles an empty list" $
--         comp (C.corelist []) `shouldBe` Atom (gref "KNIL")
--       it "compiles an singleton list" $
--         comp (C.corelist [C.int 2]) `shouldBe`
--         letrec_
--           [pc_ [V $ NativeNumber 2, gref "KNIL"] consConstructor]
--           (Atom $ L 0)
--     context "handles simple blocks" $ do
--       it "compiles an empty block" $
--         comp (C.block []) `shouldBe`
--         let_ [pc0_ nilConstructor] (appcon_ stgBlock [L 0])
--       it "compiles an simple data block" $
--         comp (C.block [C.element "a" $ C.str "a", C.element "b" $ C.str "b"]) `shouldBe`
--         asAndBs
--     context "handles catenation" $
--       it "compiles catenations with internal args" $
--       comp (C.app (C.bif "CAT") [C.int 5, C.app (C.bif "ADD") [C.int 1]]) `shouldBe`
--       let_
--         [ pc0_ $
--           thunk_ $ ann_ "" 0 $ appfn_ (gref "ADD") [V (NativeNumber 1)]
--         ]
--         (appfn_ (gref "CAT") [V (NativeNumber 5), L 0])
--     context "handles lookup" $
--       it "compiles lookup correctly" $
--       comp
--         (C.corelookup
--            (C.block [C.element "a" $ C.str "a", C.element "b" $ C.str "b"])
--            "a") `shouldBe`
--       let_
--         [pc0_ $ thunk_ $ ann_ "" 0 asAndBs]
--         (appfn_ (gref "LOOKUP") [V $ NativeSymbol "a", L 0])
--     context "manages envsize for subexprs" $
--       it "factors both free and bound into envsize for subexprs" $
--       comp
--         (C.letexp
--            [ ("k", C.lam ["x", "y"] (var "x"))
--            , ( "s"
--              , C.lam
--                  ["f", "g", "x"]
--                  (C.app
--                     (C.bif "CAT")
--                     [C.app (var "g") [var "x"], C.app (var "f") [var "x"]]))
--            ]
--            (C.app (C.bif "CAT") [C.int 5, C.app (var "s") [var "k", var "k"]])) `shouldBe`
--       letrec_
--         [ pc0_ $ lam_ 0 2 (ann_ "k" 0 $ Atom (L 0))
--         , pc0_ $
--           lam_
--             0
--             3
--             (ann_ "s" 0 $
--              let_
--                [ pc_ [L 1, L 2] $
--                  thunkn_ 2 $ ann_ "" 0 $ appfn_ (L 0) [L 1]
--                , pc_ [L 0, L 2] $
--                  thunkn_ 2 $ ann_ "" 0 $ appfn_ (L 0) [L 1]
--                ]
--                (appfn_ (gref "CAT") [L 3, L 4]))
--         ]
--         (let_
--            [ pc_ [L 1, L 0] $
--              thunkn_ 2 $ ann_ "" 0 $ appfn_ (L 0) [L 1, L 1]
--            ]
--            (appfn_ (gref "CAT") [V (NativeNumber 5), L 2]))
--   applySpec

-- asAndBs :: StgSyn
-- asAndBs = blk
--   where
--     a2a =
--       ann_ "<item>" 0 $
--       list_ 0 [V (NativeSymbol "a"), V (NativeString "a")] Nothing
--     b2b =
--       ann_ "<item>" 0 $
--       list_ 0 [V (NativeSymbol "b"), V (NativeString "b")] Nothing
--     els =
--       ann_ "<content>" 0 $
--       let_
--         [pc0_ $ thunk_ a2a, pc0_ $ thunk_ b2b]
--         (list_ 2 [L 0, L 1] Nothing)
--     blk = let_ [pc0_ $ thunk_ els] (appcon_ stgBlock [L 0])


-- applySpec :: Spec
-- applySpec =
--   describe "compilation of CoreApply" $ do
--     it "reads strictness" $
--       globalSignature "ADD" `shouldBe` [Strict, Strict]
--     it "uses cases for strict arguments" $
--       comp
--         (C.app
--            (C.bif "ADD")
--            [ C.app (C.bif "SUB") [C.int 3, C.int 2]
--            , C.app (C.bif "SUB") [C.int 3, C.int 2]
--            ]) `shouldBe`
--       force_
--         (appfn_ (gref "SUB") [V $ nat 3, V $ nat 2])
--         (force_ (appfn_ (gref "SUB") [V $ nat 3, V $ nat 2]) $
--          appfn_ (gref "ADD") [L 0, L 1])
--     it "uses lets for non-strict arguments" $
--       comp (C.app (C.bif "BLOCK") [C.corelist [C.sym "foo"]]) `shouldBe`
--       let_
--         [ pc0_ $
--           thunk_ $
--           ann_ "" 0 $
--           letrec_
--             [pc_ [V $ NativeSymbol "foo", gref "KNIL"] consConstructor]
--             (Atom $ L 0)
--         ]
--         (appfn_ (gref "BLOCK") [L 0])
--     it "combines cases and lets correctly" $
--       comp
--         (C.app
--            (C.bif "IF")
--            [ C.app (C.bif "TRUE") []
--            , C.app (C.bif "BOMB") []
--            , C.app (C.bif "BOMB") []
--            ]) `shouldBe`
--       force_
--         (appfn_ (gref "TRUE") [])
--         (let_
--            [ pc0_ $ thunk_ $ ann_ "" 0 $ appfn_ (gref "BOMB") []
--            , pc0_ $ thunk_ $ ann_ "" 0 $ appfn_ (gref "BOMB") []
--            ] $
--          appfn_ (gref "IF") [L 0, L 1, L 2])
