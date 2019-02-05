{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-|
Module      : Eucalypt.Stg.Vec
Description : Tests for environment / ref vecs
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.VecSpec
  ( main
  , spec
  ) where

import Data.Function ((&))
import Eucalypt.Stg.Vec
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Env vecs" $ do
    it "create from list" $ envSize (toVec [1, 2, 3]) `shouldBe` 3
    it "can be indexed" $ toVec [1, 2, 3] `index` 1 `shouldBe` 2
    it "can be singletons" $ (singleton 1 `index` 0) `shouldBe` 1
    it "can be split" $
      (splitVecAt 2 (toVec [1, 2, 3, 4]) & fst) `shouldBe` toVec [1, 2]
    it "can be joined" $
      toVec [1, 2, 3] <> toVec [4, 5, 6] `shouldBe` toVec [1, 2, 3, 4, 5, 6]
  describe "Ref vecs" $ do
    it "creates from list" $
      refs [L 0, G 9, V "a"] `shouldBe` refs [L 0, G 9, V "a"]
    it "creates from range" $
      range 5 7 `shouldBe` (refs [L 5, L 6] :: RefVec Char)
    it "fmaps" $
      ('x' :) <$> refs [L 0, G 9, V "a"] `shouldBe` refs [L 0, G 9, V "xa"]
    it "resolves" $
      values (toVec ["a"], toVec ["b", "c", "d"]) (refs [L 0, G 2, V "z"]) `shouldBe`
      toVec ["a", "d", "z"]
