{-# LANGUAGE TemplateHaskell #-}

module Eucalypt.Driver.Lib (getResource) where

import Data.FileEmbed (embedFile)
import qualified Data.ByteString as BS

resources :: [(String, BS.ByteString)]
resources = [("prelude", $(embedFile "prelude.eu")),
             ("build-meta", $(embedFile "package.yaml"))]

getResource :: String -> Maybe BS.ByteString
getResource n = lookup n resources
