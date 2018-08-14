{-|
Module      : Eucalypt.Stg.Intrinsics.Str
Description : Basic string built-ins for the STG evaluator
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Str
  ( split
  , match
  , matches
  , join
  ) where

import Safe (headMay)
import Control.Exception.Safe
import Eucalypt.Stg.Intrinsics.Common (returnNatList, readStrList)
import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Data.List (intercalate)
import Data.Vector ((!))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Text.Regex.PCRE.Heavy as R

toRegex :: String -> Either String R.Regex
toRegex = (`R.compileM` []) . encodeUtf8 . pack

split :: MachineState -> ValVec -> IO MachineState
split ms (ValVec args) = do
  let (StgNat (NativeString target)) = args ! 0
  let (StgNat (NativeString regex)) = args ! 1
  case toRegex regex of
    (Right r) -> returnNatList ms $ map NativeString $ R.split r target
    (Left s) -> throwM $ InvalidRegex s

match :: MachineState -> ValVec -> IO MachineState
match ms (ValVec args) = do
  let (StgNat (NativeString target)) = args ! 0
  let (StgNat (NativeString regex)) = args ! 1
  case toRegex regex of
    (Right r) ->
      returnNatList ms $
      case headMay $ R.scan r target of
        Just (m, gs) -> map NativeString (m : gs)
        Nothing -> []
    (Left s) -> throwM $ InvalidRegex s

matches :: MachineState -> ValVec -> IO MachineState
matches ms (ValVec args) = do
  let (StgNat (NativeString target)) = args ! 0
  let (StgNat (NativeString regex)) = args ! 1
  case toRegex regex of
    (Right r) -> returnNatList ms $ map (NativeString . fst) $ R.scan r target
    (Left s) -> throwM $ InvalidRegex s

join :: MachineState -> ValVec -> IO MachineState
join ms (ValVec args) = do
  let (StgAddr l) = args ! 0
  let (StgNat (NativeString s)) = args ! 1
  xs <- readStrList ms l
  return $ setCode ms (ReturnLit $ NativeString $ intercalate s xs)
