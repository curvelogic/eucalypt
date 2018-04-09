module Eucalypt.Core.PrettySpec (main, spec)
where

import Control.Monad.Supply
import Eucalypt.Core.Syn
import Eucalypt.Core.Pretty
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =

  describe "PPrint" $

    it "prints applications" $
      pprint expr  `shouldBe` "(($+ 2) 5)"
        where expr = (CoreApp (CoreApp (CoreVar  "+") (CorePrim (Int 2))) (CorePrim (Int 5)))
