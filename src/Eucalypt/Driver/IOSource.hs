module Eucalypt.Driver.IOSource where

import Eucalypt.Core.Syn
import System.Posix.Time (epochTime)
import System.Environment (getEnvironment)

euEnv :: IO CoreExpr
euEnv = getEnvironment >>= \e -> return $ CoreBlock . CoreList $ map kv e
  where
    kv (k, v) =
      CoreList [CorePrim (CoreSymbol k), CorePrim (CoreString v)]

euUnixTimestamp :: IO CoreExpr
euUnixTimestamp = epochTime >>= \t -> return $ (CorePrim . CoreInt . toInteger . fromEnum) t

prepareIOUnit :: IO CoreExpr
prepareIOUnit = do
  env <- euEnv
  et <- euUnixTimestamp
  return $
    letexp
      [ ( "__io"
        , letexp [("ENV", env), ("EPOCHTIME", et)] $
          block
            [element "ENV" $ var "ENV", element "EPOCHTIME" $ var "EPOCHTIME"])
      ]
      (block [])
