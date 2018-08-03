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

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Compiler
import Text.PrettyPrint as P
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "litList_" $
    it "creates list of natives" $ do
      let l = litList_ (map NativeInt [0, 1, 2])
      (P.render . prettify) l `shouldBe`
        "letrec {} \\ 0 0 -> C|0 \n       {2, E[0]} \\ 2 0 -> C|1 E[0] E[1]\n       {1, E[1]} \\ 2 0 -> C|1 E[0] E[1]\n       {0, E[2]} \\ 2 0 -> C|1 E[0] E[1]\n in E[2] "
