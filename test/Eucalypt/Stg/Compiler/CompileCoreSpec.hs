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
