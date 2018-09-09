{-# OPTIONS_GHC -fno-warn-type-defaults #-}
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
  , strNat
  , strSym
  ) where

import Safe (headMay)
import Eucalypt.Stg.Intrinsics.Common (returnNatList, readStrList)
import Eucalypt.Stg.Error
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Machine
import Data.List (intercalate)
import Data.Scientific (floatingOrInteger)
import Data.Vector ((!))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Text.Regex.PCRE.Heavy as R

toRegex :: String -> Either String R.Regex
toRegex = (`R.compileM` []) . encodeUtf8 . pack



-- | __SPLIT(s, re)
split :: MachineState -> ValVec -> IO MachineState
split ms (ValVec args) = do
  let (StgNat (NativeString target) _) = args ! 0
  let (StgNat (NativeString regex) _) = args ! 1
  if null regex
    then returnNatList ms [NativeString target]
    else case toRegex regex of
           (Right r) -> returnNatList ms $ map NativeString $ R.split r target
           (Left s) -> throwIn ms $ InvalidRegex s



-- | __MATCH(s, re)
match :: MachineState -> ValVec -> IO MachineState
match ms (ValVec args) = do
  let (StgNat (NativeString target) _) = args ! 0
  let (StgNat (NativeString regex) _) = args ! 1
  case toRegex regex of
    (Right r) ->
      returnNatList ms $
      case headMay $ R.scan r target of
        Just (m, gs) -> map NativeString (m : gs)
        Nothing -> []
    (Left s) -> throwIn ms $ InvalidRegex s



-- | __MATCHES(s, re)
matches :: MachineState -> ValVec -> IO MachineState
matches ms (ValVec args) = do
  let (StgNat (NativeString target) _) = args ! 0
  let (StgNat (NativeString regex) _) = args ! 1
  case toRegex regex of
    (Right r) -> returnNatList ms $ map (NativeString . fst) $ R.scan r target
    (Left s) -> throwIn ms $ InvalidRegex s



-- | __JOIN(els, sep)
join :: MachineState -> ValVec -> IO MachineState
join ms (ValVec args) = do
  let (StgAddr l) = args ! 0
  let (StgNat (NativeString s) _) = args ! 1
  xs <- readStrList ms l
  return $ setCode ms (ReturnLit (NativeString $ intercalate s xs) Nothing)



strNat :: MachineState -> ValVec -> IO MachineState
strNat ms (ValVec args) =
  return $
  setCode ms $
  (`ReturnLit` Nothing) $
  NativeString $
  let (StgNat n _) = args ! 0
   in case n of
        NativeNumber sc ->
          case floatingOrInteger sc of
            Left f -> show f
            Right i -> show i
        NativeString s -> s
        NativeSymbol s -> s
        NativeBool b ->
          if b
            then "true"
            else "false"



strSym :: MachineState -> ValVec -> IO MachineState
strSym ms (ValVec args) =
  let (StgNat (NativeString nm) _) = args ! 0
   in return $ setCode ms $ (`ReturnLit` Nothing) $ NativeSymbol nm
