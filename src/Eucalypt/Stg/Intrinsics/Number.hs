{-|
Module      : Eucalypt.Stg.Intrinsics.Number
Description : Basic number built ins for the STG evaluator
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Number
  ( parse
  ) where

import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Eucalypt.Syntax.Ast (PrimitiveLiteral(..))
import Eucalypt.Syntax.ParseExpr (number)
import Data.Scientific
import Data.Sequence ((!?))
import qualified Text.Megaparsec as M


toNative :: PrimitiveLiteral -> Maybe Native
toNative (VInt n) = Just . NativeNumber . fromIntegral $ n
toNative (VFloat f) = Just . NativeNumber . fromFloatDigits $ f
toNative _ = Nothing

-- | Parse text into a number
parse :: MachineState -> ValVec -> IO MachineState
parse ms (ValVec args) = do
  let (Just (StgNat (NativeString text) _)) = args !? 0
  let num = M.parseMaybe number text >>= toNative
  case num of
    (Just n) -> return $ setCode ms (ReturnLit n Nothing)
    Nothing -> throwIn ms (InvalidNumber text)
