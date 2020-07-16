{-# LANGUAGE TemplateHaskell #-}

module Eucalypt.Driver.Lib (getResource) where

import Data.FileEmbed (embedFile)
import qualified Data.ByteString as BS

resources :: [(String, BS.ByteString)]
resources = [("prelude", $(embedFile "lib/prelude.eu")),
             ("build-meta", $(embedFile "build-metadata.yaml")),
             ("package", $(embedFile "package.yaml"))]

getResource :: String -> Maybe BS.ByteString
getResource n = lookup n resources
