{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Eucalypt.Stg.Compiler.ApplicationSpec
Description : Tests for application compilation helpers
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Compiler.ApplicationSpec (main, spec)
where

import Eucalypt.Core.AnonSyn
import Eucalypt.Stg.Compiler.Application
import Eucalypt.Stg.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "sorting refs in ArgCase" $ do
    it "renumbers" $
      sortArgCaseRefs [EnvRef $ L 5, EnvRef $ L 4 :: ArgCase String] `shouldBe`
      ([L 5, L 4], EnvRef <$> [L 0, L 1])
    it "ignores scrutinees" $
      sortArgCaseRefs [EnvRef $ L 5, Scrutinee Nothing (var "x")] `shouldBe`
      ([L 5], [EnvRef $ L 0, Scrutinee Nothing (var "x")])
    describe "numbering closures" $
      it "numbers correctly" $
      numberPreClosures
        ( [ EnvRef $ L 0
          , Closure Nothing (var "x")
          , Scrutinee Nothing (var "z")
          , Closure Nothing (var "y")
          , EnvRef $ L 1
          ]
        , 100) `shouldBe`
      ( [ EnvRef $ L 0
        , Closure (Just 100) (var "x")
        , Scrutinee Nothing (var "z")
        , Closure (Just 101) (var "y")
        , EnvRef $ L 1
        ]
      , 102)
    describe "numbering scrutinees" $
      it "numbers correctly" $
      numberScrutinees
        ( [ EnvRef $ L 0
          , Closure Nothing (var "x")
          , Scrutinee Nothing (var "z")
          , Closure Nothing (var "y")
          , EnvRef $ L 1
          ]
        , 100) `shouldBe`
      ( [ EnvRef $ L 0
        , Closure Nothing (var "x")
        , Scrutinee (Just 100) (var "z")
        , Closure Nothing (var "y")
        , EnvRef $ L 1
        ]
      , 101)
