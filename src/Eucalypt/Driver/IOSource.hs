{-|
Module      : Eucalypt.Driver.IOSource
Description : Source of environment or IO data for import
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Driver.IOSource where

import Eucalypt.Core.SourceMap
import Eucalypt.Core.Syn
import Eucalypt.Core.Unit
import System.Posix.Time (epochTime)
import System.Environment (getEnvironment)

euEnv :: IO CoreExpr
euEnv = getEnvironment >>= \e -> return $ anon CoreBlock . anon CoreList $ map kv e
  where
    kv (k, v) =
      anon CoreList [anon CorePrim (CoreSymbol k), anon CorePrim (CoreString v)]

euUnixTimestamp :: IO CoreExpr
euUnixTimestamp =
  epochTime >>= \t ->
    return $ (anon CorePrim . CoreInt . toInteger . fromEnum) t

prepareIOUnit :: IO TranslationUnit
prepareIOUnit = do
  env <- euEnv
  et <- euUnixTimestamp
  return $
    specialUnit $
    anon
      letexp
      [ ( "__io"
        , anon letexp [("ENV", env), ("EPOCHTIME", et)] $
          anon
            block
            [ anon element "ENV" $ anon var "ENV"
            , anon element "EPOCHTIME" $ anon var "EPOCHTIME"
            ])
      ]
      (anon block [])
