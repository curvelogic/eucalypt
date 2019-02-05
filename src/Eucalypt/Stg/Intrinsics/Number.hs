{-|
Module      : Eucalypt.Stg.Intrinsics.Number
Description : Basic number built ins for the STG evaluator
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Number
  ( intrinsics
  ) where

import Data.Scientific
import Eucalypt.Stg.Error
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Syntax.Ast (PrimitiveLiteral(..))
import Eucalypt.Syntax.ParseExpr (number)
import qualified Text.Megaparsec as M


intrinsics :: [IntrinsicInfo]
intrinsics = [IntrinsicInfo "NUMPARSE" 1 (invoke parse)]

toNative :: PrimitiveLiteral -> Maybe Native
toNative (VInt n) = Just . NativeNumber . fromIntegral $ n
toNative (VFloat f) = Just . NativeNumber . fromFloatDigits $ f
toNative _ = Nothing

-- | Parse text into a number
parse :: MachineState -> String -> IO MachineState
parse ms text = do
  let num = M.parseMaybe number text >>= toNative
  case num of
    (Just n) -> return $ setCode ms (ReturnLit n Nothing)
    Nothing -> throwIn ms (InvalidNumber text)
