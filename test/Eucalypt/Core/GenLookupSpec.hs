{-# LANGUAGE LambdaCase #-}

module Eucalypt.Core.GenLookupSpec
  ( main
  , spec
  ) where

import qualified Eucalypt.Core.AnonSyn as ASyn
import Eucalypt.Core.Desugar
import Eucalypt.Core.GenLookup
import qualified Eucalypt.Core.Syn as Syn
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Generalised lookup transformations" $ do
    it "handles static generalised lookup" $
      processGenLookup
        [ ASyn.letblock [("a", ASyn.int 1)] $
          ASyn.block [ASyn.element "a" (ASyn.var "a")]
        , Syn.lookupOp
        , ASyn.soup [ASyn.corename "a", ASyn.corename "+", ASyn.corename "a"]
        ] `shouldBe`
      [ ASyn.letexp [("a", ASyn.int 1)] $
        ASyn.soup [ASyn.corename "a", ASyn.corename "+", ASyn.corename "a"]
      ]
    it "handles simple lookup on blocks (as static lookup)" $
      (processGenLookup . varifyLookupTargets)
        [ ASyn.letblock [("a", ASyn.int 1)] $
          ASyn.block [ASyn.element "a" (ASyn.var "a")]
        , Syn.lookupOp
        , ASyn.corename "a"
        ] `shouldBe`
      [ASyn.letexp [("a", ASyn.int 1)] $ ASyn.var "a"]
    it "handles simple dynamic transformations" $
      processGenLookup
        [ ASyn.var "opaque"
        , Syn.lookupOp
        , ASyn.soup [ASyn.var "a", ASyn.var "+", ASyn.var "a"]
        ] `shouldBe`
      [ ASyn.var "opaque"
      , Syn.lookupOp
      , ASyn.lam ["___"] $
        ASyn.soup
          [ ASyn.dynlookup (ASyn.var "___") "a" (ASyn.var "a")
          , ASyn.var "+"
          , ASyn.dynlookup (ASyn.var "___") "a" (ASyn.var "a")
          ]
      ]
