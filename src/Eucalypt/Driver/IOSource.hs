{-|
Module      : Eucalypt.Driver.IOSource
Description : Source of environment or IO data for import
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Driver.IOSource where

import Data.Time.LocalTime
import Eucalypt.Core.AnonSyn
import Eucalypt.Core.Unit
import System.Posix.Time (epochTime)
import System.Environment (getEnvironment)

euEnv :: IO CoreExpr
euEnv = getEnvironment >>= \e -> return $ block $ map kv e
  where
    kv (k, v) = element k $ str v

euUnixTimestamp :: IO CoreExpr
euUnixTimestamp = epochTime >>= \t -> return $ (int . toInteger . fromEnum) t

euTimeZone :: IO CoreExpr
euTimeZone = do
  tz@TimeZone {timeZoneName = name} <- getCurrentTimeZone
  let offset = timeZoneOffsetString tz
  return $ block [element "name" $ str name, element "offset" $ str offset]

prepareIOUnit :: IO TranslationUnit
prepareIOUnit = do
  env <- euEnv
  et <- euUnixTimestamp
  tz <- euTimeZone
  return $
    specialUnit $
    letexp
      [ ( "__io"
        , letexp [("ENV", env), ("EPOCHTIME", et), ("TZ", tz)] $
          block
            [ element "ENV" $ var "ENV"
            , element "EPOCHTIME" $ var "EPOCHTIME"
            , element "TZ" $ var "TZ"
            ])
      ]
      (block [])
