{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Eucalypt.Stg.Globals.ListSpec
Description : Tests for list globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.ListSpec
  ( main
  , spec
  ) where

import Eucalypt.Stg.Compiler
import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.StgTestUtil
import Test.Hspec

main :: IO ()
main = hspec spec

extractsHead :: StgSyn
extractsHead =
  let_
    [ pc0_ $
      thunk_ $ list_ 0 [Global "TRUE", Global "FALSE", Global "FALSE"] Nothing
    ] $
  appfn_ (Global "HEAD") [Local 0]

extractsTail :: StgSyn
extractsTail =
  letrec_
    [ pc0_ $
      thunk_ $ list_ 0 [Global "TRUE", Global "FALSE", Global "FALSE"] Nothing
    , pc_ [Local 0] $ thunkn_ 1 $ appfn_ (Global "TAIL") [Local 0]
    , pc0_ $ thunk_ $ list_ 0 [Global "FALSE", Global "FALSE"] Nothing
    ] $
  appfn_ (Global "EQ") [Local 1, Local 2]

conses :: StgSyn
conses =
  letrec_
    [ pc0_ $
      thunk_ $
      list_ 0 [Literal $ nat 1, Literal $ nat 2, Literal $ nat 3] Nothing
    , pc_ [Local 0] $
      thunkn_ 1 $ appfn_ (Global "CONS") [Literal $ nat 0, Local 0]
    , pc0_ $
      thunk_ $
      list_
        0
        [Literal $ nat 0, Literal $ nat 1, Literal $ nat 2, Literal $ nat 3]
        Nothing
    ] $
  appfn_ (Global "EQ") [Local 1, Local 2]

consesWithEmpty :: StgSyn
consesWithEmpty =
  let_
    [ pc0_ $ thunk_ $ appfn_ (Global "CONS") [Literal $ nat 0, Global "KNIL"]
    , pc0_ $ thunk_ $ list_ 0 [Literal $ nat 0] Nothing
    ] $
  appfn_ (Global "EQ") [Local 0, Local 1]

consesWithListValuedCall :: StgSyn
consesWithListValuedCall =
  letrec_
    [ pc0_ $
      thunk_ $
      list_ 0 [Literal $ nat 1, Literal $ nat 2, Literal $ nat 3] Nothing
    , pc_ [Local 0] $ thunkn_ 1 $ appfn_ (Global "TAIL") [Local 0]
    , pc_ [Local 1] $
      thunkn_ 1 $ appfn_ (Global "CONS") [Literal $ nat 0, Local 0]
    , pc0_ $
      thunk_ $
      list_ 0 [Literal $ nat 0, Literal $ nat 2, Literal $ nat 3] Nothing
    ] $
  appfn_ (Global "EQ") [Local 2, Local 3]

headEmptyList :: StgSyn
headEmptyList = appfn_ (Global "HEAD") [Global "KNIL"]


spec :: Spec
spec =
  describe "List builtins" $ do
    it "extracts head" $ returnsTrue <$> test extractsHead `shouldReturn` True
    it "extracts tail" $ returnsTrue <$> test extractsTail `shouldReturn` True
    it "conses" $ returnsTrue <$> test conses `shouldReturn` True
    it "conses with empty" $
      returnsTrue <$> test consesWithEmpty `shouldReturn` True
    it "conses with list-valued call" $
      returnsTrue <$> test consesWithListValuedCall `shouldReturn` True
    it "throws on head of empty list" $
      test headEmptyList `shouldThrow`
      (\(e :: StgException) -> stgExcError e == Panic "Head of empty list")


-- old tests to reimplement?

  --   it "throws when head-ing []" $
  --     runInterpreter (euHead whnfM [CoreList []]) `shouldSatisfy` isLeft
  --   it "throws when tail-ing []" $
  --     runInterpreter (euTail whnfM [CoreList []]) `shouldSatisfy` isLeft
  --   it "throws consing a non-list tail" $
  --     runInterpreter (euCons whnfM [int 0, int 1]) `shouldSatisfy` isLeft

  --   it "judges head([1,2,3])=1" $
  --     runRightInterpreter
  --       (whnfM (app (bif "HEAD") [CoreList [int 1, int 2, int 3]])) `shouldBe`
  --     int 1
  --   it "judges head(cons(h,t))=h" $
  --     runRightInterpreter
  --       (whnfM
  --          (app
  --             (bif "EQ")
  --             [ app
  --                 (bif "HEAD")
  --                 [app (bif "CONS") [int 1, CoreList [int 2, int 3]]]
  --             , int 1
  --             ])) `shouldBe`
  --     corebool True
  --   it "judges head(cons(head(l), tail(l)))=head(l)" $
  --     runRightInterpreter
  --       (whnfM
  --          (app
  --             (bif "HEAD")
  --             [ app
  --                 (bif "CONS")
  --                 [ app (bif "HEAD") [CoreList [int 1, int 2, int 3]]
  --                 , app (bif "TAIL") [CoreList [int 1, int 2, int 3]]
  --                 ]
  --             ])) `shouldBe`
  --     int 1
  --   it "judges cons(head(l), tail(l))=l" $
  --     runRightInterpreter
  --       (forceDataStructures
  --          whnfM
  --          (app
  --             (bif "CONS")
  --             [ app (bif "HEAD") [CoreList [int 1, int 2, int 3]]
  --             , app (bif "TAIL") [CoreList [int 1, int 2, int 3]]
  --             ])) `shouldBe`
  --     CoreList [int 1, int 2, int 3]
  --   it "judges eq(l,(cons(head(l), tail(l))))=true" $
  --     runRightInterpreter
  --       (whnfM
  --          (app
  --             (bif "EQ")
  --             [ CoreList [int 1, int 2, int 3]
  --             , app
  --                 (bif "CONS")
  --                 [ app (bif "HEAD") [CoreList [int 1, int 2, int 3]]
  --                 , app (bif "TAIL") [CoreList [int 1, int 2, int 3]]
  --                 ]
  --             ])) `shouldBe`
  --     corebool True
