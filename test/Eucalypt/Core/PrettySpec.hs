module Eucalypt.Core.PrettySpec (main, spec)
where

import Eucalypt.Core.Syn
import Eucalypt.Core.Pretty
import Test.Hspec

main :: IO ()
main = hspec spec


sample :: CoreExpr
sample =
  letexp
    [ ( "take"
      , lam
          ["n", "l"]
          (soup
             [ bif "IF"
             , infixl_ 90 (bif "*CALL*")
             , args
                 [ soup [var "n", var "zero?"]
                 , CoreList []
                 , soup
                     [ var "cons"
                     , infixl_ 90 (bif "*CALL*")
                     , args
                         [ soup [var "l", var "head"]
                         , soup
                             [ var "take"
                             , infixl_ 90 (bif "*CALL*")
                             , args
                                 [ soup [var "n", var "dec"]
                                 , soup [var "l", var "tail"]
                                 ]
                             ]
                         ]
                     ]
                 ]
             ]))
    ]
    (block [element "take" $ var "take"])


spec :: Spec
spec =
  describe "PPrint" $ do
    it "prints applications" $
      pprint (app (var "+") [int 2, int 5]) `shouldBe` "+(2, 5)"
    it "reconstructs bound names in lambdas" $
      pprint (lam ["foo"] (var "foo")) `shouldBe` "(\\ foo -> foo)"
    it "reconstructs bound names in lets" $
      pprint
        (letexp
           [("foo", int 2), ("bar", int 3)]
           (app (var "+") [var "foo", var "bar"])) `shouldBe`
      "let foo = 2\n    bar = 3\n    in +(foo, bar)"
    it "reconstructs bound names even for unused bindings in lets" $
      pprint (letexp [("foo", int 2), ("bar", int 3)] (var "foo")) `shouldBe`
      "let foo = 2\n    bar = 3\n    in foo"
    it "prints sample" $
      pprint sample `shouldBe`
      "let take = (\\ n l -> (__IF __*CALL* ((n zero?), [], (cons __*CALL* ((l head), (take __*CALL* ((n dec), (l tail))))))))\n    in {[[:take,take]]}"
