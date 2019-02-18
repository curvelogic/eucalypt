{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Source.CsvSource
Description : Tests for CSV ingestion
Copyright   : (c) Greg Hawkins, 2017
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Source.CsvSourceSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.AnonSyn
import Eucalypt.Source.CsvSource
import Data.Text.Lazy
import Data.Text.Lazy.Encoding
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "CSV source" $ do
    it "reads blocks" $
      parseCsv (encodeUtf8 $ pack "a,b,c\n1,2,3\n4,5,6") `shouldReturn`
      corelist
        [ block
            [ element "a" $ str "1"
            , element "b" $ str "2"
            , element "c" $ str "3"
            ]
        , block
            [ element "a" $ str "4"
            , element "b" $ str "5"
            , element "c" $ str "6"
            ]
        ]
    it "handles quotes, preserves spaces and ignores trailing new lines" $
      parseCsv (encodeUtf8 $ pack "a,b,c\n\"hello\", world ,   stuff   \n\n\n") `shouldReturn`
      corelist
        [ block
            [ element "a" $ str "hello"
            , element "b" $str " world "
            , element "c" $ str "   stuff   "
            ]
        ]
    it "handles unicode" $
      parseCsv (encodeUtf8 $ pack "Μ,Ν,Ξ\nμ,ν,ξ") `shouldReturn`
      corelist
        [ block
            [ element "Μ" $ str "μ"
            , element "Ν" $str "ν"
            , element "Ξ" $ str "ξ"
            ]
        ]
    it "handles empty fields" $
      parseCsv (encodeUtf8 $ pack "x,y,z\n,,\n") `shouldReturn`
      corelist
        [ block
            [ element "x" $ str ""
            , element "y" $str ""
            , element "z" $ str ""
            ]
        ]
