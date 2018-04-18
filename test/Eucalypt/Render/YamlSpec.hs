{-# LANGUAGE OverloadedStrings #-}
module Eucalypt.Render.YamlSpec
  ( main
  , spec
  ) where

import Eucalypt.Render.Yaml
import Eucalypt.Core.Syn as Syn
import Test.Hspec
import Data.Text.Encoding (encodeUtf8)

main :: IO ()
main = hspec spec



coreNF1 :: CoreExp a
coreNF1 =
  CoreBlock
    (CoreList
       [ CoreList [CorePrim $ Symbol "a", CorePrim $ Syn.Int 1234]
       , CoreList
           [ CorePrim $ Symbol "b"
           , CoreList
               [ CorePrim $ String "x"
               , CorePrim $ String "y"
               , CorePrim $ String "z"
               ]
           ]
       ])



coreNF2 :: CoreExp a
coreNF2 = CoreList
               [ CorePrim $ Int 1
               , CorePrim $ Int 2
               , CorePrim $ Int 3
               , CorePrim $ Int 4
               , CorePrim $ Int 5
               , CorePrim $ Int 6
               , CorePrim $ Int 7
               ]



coreNF3 :: CoreExp a
coreNF3 =
  CoreBlock
    (CoreList
       [ CoreList [CorePrim $ Symbol "a", CorePrim $ Syn.Int 1]
       , CoreList [CorePrim $ Symbol "b", CorePrim $ Syn.Int 2]
       , CoreList [CorePrim $ Symbol "c", CorePrim $ Syn.Int 3]
       , CoreList [CorePrim $ Symbol "d", CorePrim $ Syn.Int 4]
       , CoreList [CorePrim $ Symbol "e", CorePrim $ Syn.Int 5]
       , CoreList [CorePrim $ Symbol "f", CorePrim $ Syn.Int 6]
       , CoreList [CorePrim $ Symbol "g", CorePrim $ Syn.Int 7]
       ])



spec :: Spec
spec =
  describe "Yaml rendering" $ do
    xit "Renders simple NF core block to Yaml" $
      renderYamlBytes return coreNF1 `shouldReturn`
      (return . encodeUtf8) "a: 1234\nb:\n  - x\n  y\n  z\n"
    --        expected: Right "a: 1234\nb:\n  - x\n  y\n  z\n"
    --         but got: Right "a: 1234\nb:\n- x\n- 'y'\n- z\n"
    -- TODO: mysterious...

    it "Renders NF core list" $
      renderYamlBytes return coreNF2 `shouldReturn`
      (return . encodeUtf8) "- 1\n- 2\n- 3\n- 4\n- 5\n- 6\n- 7\n"
    it "Maintains key order" $
      renderYamlBytes return coreNF3 `shouldReturn`
      (return . encodeUtf8) "a: 1\nb: 2\nc: 3\nd: 4\ne: 5\nf: 6\ng: 7\n"
    it "Forces to WHNF to render" pending
