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
       [ CoreList [sym "a", int 1234]
       , CoreList [sym "b", CoreList [str "x", str "y", str "z"]]
       ])



coreNF2 :: CoreExp a
coreNF2 = CoreList
               [ int 1
               , int 2
               , int 3
               , int 4
               , int 5
               , int 6
               , int 7
               ]



coreNF3 :: CoreExp a
coreNF3 =
  CoreBlock
    (CoreList
       [ CoreList [sym "a", int 1]
       , CoreList [sym "b", int 2]
       , CoreList [sym "c", int 3]
       , CoreList [sym "d", int 4]
       , CoreList [sym "e", int 5]
       , CoreList [sym "f", int 6]
       , CoreList [sym "g", int 7]
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
        (CoreBlock (CoreList [CoreList [sym "a", bif "NULL"]])) `shouldReturn`
      (return . encodeUtf8) "a: null\n"
    it "omits builtin declarations from render" $
      renderYamlBytes
        whnfM
        (CoreBlock (CoreList [CoreList [sym "a", bif "OR"]])) `shouldReturn`
      (return . encodeUtf8) "{}\n"
