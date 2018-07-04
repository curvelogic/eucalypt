{-# LANGUAGE OverloadedStrings #-}
module Eucalypt.Render.JsonSpec
  ( main
  , spec
  ) where

import Data.Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (fromRight)
import Eucalypt.Core.Error
import Eucalypt.Core.EvalByName
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Eucalypt.Render.Classes
import Eucalypt.Render.Json
import Test.Hspec

right :: Either l r -> r
right = fromRight undefined

main :: IO ()
main = hspec spec


coreNF1 :: CoreExp a
coreNF1 =
  block
       [ element "a" $ int 1234
       , element "b" $ CoreList [str "x", str "y", str "z"]
       ]



coreNF2 :: CoreExp a
coreNF2 = CoreList [int 1, int 2, int 3, int 4, int 5, int 6, int 7]



_coreNF3 :: CoreExp a
_coreNF3 =
  block
    [ element "a" $ int 1
    , element "b" $ int 2
    , element "c" $ int 3
    , element "d" $ int 4
    , element "e" $ int 5
    , element "f" $ int 6
    , element "g" $ int 7
    ]

jsonSampleNull :: CoreExp a
jsonSampleNull = block [element "a" $ bif "NULL"]

render :: WhnfEvaluator -> CoreExpr -> IO (Either EvaluationError BS.ByteString)
render = renderBytes (JsonConfig { jsonPretty = True })

spec :: Spec
spec =
  describe "JSON rendering" $ do
    it "Renders simple NF core block to readable JSON" $
      (decode . BL.fromStrict . right <$> render return coreNF1) `shouldReturn`
      (decode "{\"a\": 1234, \"b\": [\"x\", \"y\", \"z\"]}" :: Maybe Value)
    it "Renders NF core list to readable JSON" $
      (decode . BL.fromStrict . right <$> render return coreNF2) `shouldReturn`
      (decode "[1, 2, 3, 4, 5, 6, 7]" :: Maybe Value)
    it "Maintains key order" pending
    it "Forces to WHNF to render" pending
    it "Renders and evals { a: __NULL }" $
      (decode . BL.fromStrict . right <$> render whnfM jsonSampleNull) `shouldReturn`
      (decode "{ \"a\": null }" :: Maybe Value)
    it "omits builtin declarations from render" $
      (decode . BL.fromStrict . right <$> render whnfM (block [element "a" $ bif "OR"])) `shouldReturn`
      (decode "{}\n" :: Maybe Value)
