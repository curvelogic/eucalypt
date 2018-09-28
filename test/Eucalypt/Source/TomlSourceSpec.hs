{-# LANGUAGE OverloadedStrings #-}

module Eucalypt.Source.TomlSourceSpec
  ( main
  , spec
  ) where

import Eucalypt.Core.Syn
import Eucalypt.Source.TomlSource
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Toml parser" $
  it "parses basic toml snippet" $
  parseTomlData "foo = \"bar\"\n\n[a]\nx=3\ny = 4\nz.p = 12\n[r.s.t]\nf.f=8\n" `shouldReturn`
  block
    [ element "foo" $ str "bar"
    , element "a" $
      block
        [ element "x" $ int 3
        , element "z" $ block [element "p" $ int 12]
        , element "y" $ int 4
        ]
    , element "r" $
      block
        [ element "s" $
          block
            [element "t" $ block [element "f" $ block [element "f" $ int 8]]]
        ]
    ]
