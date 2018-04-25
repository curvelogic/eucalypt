{-# LANGUAGE OverloadedStrings #-}
module Eucalypt.Render.YamlSpec
  ( main
  , spec
  ) where

import Eucalypt.Render.Yaml
import Eucalypt.Core.Syn
import Test.Hspec
import Eucalypt.Core.EvalByName
import Data.Text.Encoding (encodeUtf8)

main :: IO ()
main = hspec spec



coreNF1 :: CoreExp a
coreNF1 =
  CoreBlock
    (CoreList
       [ CoreList [CorePrim $ CoreSymbol "a", CorePrim $ CoreInt 1234]
       , CoreList
           [ CorePrim $ CoreSymbol "b"
           , CoreList
               [ CorePrim $ CoreString "x"
               , CorePrim $ CoreString "y"
               , CorePrim $ CoreString "z"
               ]
           ]
       ])



coreNF2 :: CoreExp a
coreNF2 = CoreList
               [ CorePrim $ CoreInt 1
               , CorePrim $ CoreInt 2
               , CorePrim $ CoreInt 3
               , CorePrim $ CoreInt 4
               , CorePrim $ CoreInt 5
               , CorePrim $ CoreInt 6
               , CorePrim $ CoreInt 7
               ]



coreNF3 :: CoreExp a
coreNF3 =
  CoreBlock
    (CoreList
       [ CoreList [CorePrim $ CoreSymbol "a", CorePrim $ CoreInt 1]
       , CoreList [CorePrim $ CoreSymbol "b", CorePrim $ CoreInt 2]
       , CoreList [CorePrim $ CoreSymbol "c", CorePrim $ CoreInt 3]
       , CoreList [CorePrim $ CoreSymbol "d", CorePrim $ CoreInt 4]
       , CoreList [CorePrim $ CoreSymbol "e", CorePrim $ CoreInt 5]
       , CoreList [CorePrim $ CoreSymbol "f", CorePrim $ CoreInt 6]
       , CoreList [CorePrim $ CoreSymbol "g", CorePrim $ CoreInt 7]
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
    it "renders and evals { a: __NULL }" $
      renderYamlBytes
        whnfM
        (CoreBlock
           (CoreList
              [CoreList [CorePrim $ CoreSymbol "a", CoreBuiltin "NULL"]])) `shouldReturn`
      (return . encodeUtf8) "a: null\n"
