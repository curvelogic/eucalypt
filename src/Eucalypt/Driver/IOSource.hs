{-|
Module      : Eucalypt.Driver.IOSource
Description : Source of environment or IO data for import
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Driver.IOSource where

import Eucalypt.Core.Syn
import Eucalypt.Core.Unit
import System.Posix.Time (epochTime)
import System.Environment (getEnvironment)

euEnv :: IO CoreExpr
euEnv = getEnvironment >>= \e -> return $ CoreBlock . CoreList $ map kv e
  where
    kv (k, v) =
      CoreList [CorePrim (CoreSymbol k), CorePrim (CoreString v)]

euUnixTimestamp :: IO CoreExpr
euUnixTimestamp = epochTime >>= \t -> return $ (CorePrim . CoreInt . toInteger . fromEnum) t

prepareIOUnit :: IO TranslationUnit
prepareIOUnit = do
  env <- euEnv
  et <- euUnixTimestamp
  return $ dataUnit $
    letexp
      [ ( "__io"
        , letexp [("ENV", env), ("EPOCHTIME", et)] $
          block
            [element "ENV" $ var "ENV", element "EPOCHTIME" $ var "EPOCHTIME"])
      ]
      (block [])
