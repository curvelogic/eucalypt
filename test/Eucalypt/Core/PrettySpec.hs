module Eucalypt.Core.PrettySpec (main, spec)
where

import Eucalypt.Core.Syn
import Eucalypt.Core.Pretty
import Text.PrettyPrint (render)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do

  describe "PPrint" $ do

    it "prints applications" $

      (render . prepare) (App (App (Var $ fromStr "+") (Prim (Int 2))) (Prim (Int 5))) `shouldBe` "(($+ i:2) i:5)"
