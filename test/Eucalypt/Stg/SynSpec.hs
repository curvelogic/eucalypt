{-|
Module      : Eucalypt.Stg.Syn
Description : Tests for STG syntax helpers
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Stg.SynSpec (main, spec)
where

import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "dsl" $ do
    it "creates constructor applications" $
      synCallable (appcon_ 9 [V (NativeString "a")]) `shouldBe` Con 9
    it "creates case statements" $
      synScrutinee (force_ (appfn_ (L 0) [L 1, L 2]) (appfn_ (L 0) [L 4])) `shouldBe`
      appfn_ (L 0) [L 1, L 2]
    it "creates thunks" $
      lamUpdate (thunk_ (appfn_ (G 0) [L 1])) `shouldBe` True
