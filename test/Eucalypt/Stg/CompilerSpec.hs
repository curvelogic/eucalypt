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

  describe "compilation" $
    context "compiles primitives" $ do
      it "compiles ints" $
        compile 0 emptyContext (C.int 2 :: C.CoreExpr) `shouldBe` (Atom (Literal (NativeInt 2)))
      it "compiles strings" $
        compile 0 emptyContext (C.str "foo" :: C.CoreExpr) `shouldBe` (Atom (Literal (NativeString "foo")))
      it "compiles strings" $
        compile 0 emptyContext (C.corebool False :: C.CoreExpr) `shouldBe` (Atom (Literal (NativeBool False)))
