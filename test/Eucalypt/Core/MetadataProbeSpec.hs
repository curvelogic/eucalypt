module Eucalypt.Core.MetadataProbeSpec
  ( main
  , spec
  ) where

import Data.Either (fromRight)
import Eucalypt.Core.Interpreter
import Eucalypt.Core.MetadataProbe
import Eucalypt.Core.Syn
import Test.Hspec

main :: IO ()
main = hspec spec

blockA :: CoreExpr
blockA =
  letexp [("a", int 5), ("b", int 6)] $
  block
    [ CoreMeta (sym "a") $ element "a" $ var "a"
    , CoreMeta (sym "b") $ element "b" $ var "b"
    ]

blockB :: CoreExpr
blockB =
  letexp [("c", int 5), ("d", int 6)] $
  block
    [ CoreMeta (sym "c") $ element "c" $ var "c"
    , CoreMeta (sym "d") $ element "d" $ var "d"
    ]

rootBlock :: CoreExpr
rootBlock =
  letexp [("block-a", blockA), ("block-b", blockB)] $
  block
    [ CoreMeta (sym "A") $ element "block-a" $ var "block-a"
    , CoreMeta (sym "B") $ element "block-b" $ var "block-b"
    ]

annotatedPaths :: [(CoreBindingName, CoreExpr)]
annotatedPaths =
  [ ("block-a", CorePrim (CoreSymbol "A"))
  , ("block-a.a", CorePrim (CoreSymbol "a"))
  , ("block-a.b", CorePrim (CoreSymbol "b"))
  , ("block-b", CorePrim (CoreSymbol "B"))
  , ("block-b.c", CorePrim (CoreSymbol "c"))
  , ("block-b.d", CorePrim (CoreSymbol "d"))
  ]

process :: CoreExpr -> (CoreExpr, [(String, CoreExpr)])
process e = (fromRight (e, []) . runInterpreter . runMetaPass) e

spec :: Spec
spec =
  describe "Metadata pass" $
  it "Harvests annotated paths" $
  snd (process rootBlock) `shouldBe` annotatedPaths
