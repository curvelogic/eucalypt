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
  ( intrinsics
  ) where

import Data.Char (toLower, toUpper)
import Data.List (intercalate)
import Data.Scientific (floatingOrInteger)
import Data.Symbol
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import Eucalypt.Stg.Error
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common (invoke, readStrList, returnList)
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Stg.Value
import Safe (headMay)
import qualified Text.Printf as PF
import qualified Text.Regex.PCRE.Heavy as R

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "SPLIT" 2 (invoke split)
  , IntrinsicInfo "MATCH" 2 (invoke match)
  , IntrinsicInfo "MATCHES" 2 (invoke matches)
  , IntrinsicInfo "JOIN" 2 (invoke join)
  , IntrinsicInfo "LETTERS" 1 (invoke letters)
  , IntrinsicInfo "STRNAT" 1 (invoke strNat)
  , IntrinsicInfo "STRSYM" 1 (invoke strSym)
  , IntrinsicInfo "FMT" 2 (invoke fmt)
  , IntrinsicInfo "UPPER" 1 (invoke strUpper)
  , IntrinsicInfo "LOWER" 1 (invoke strLower)
  ]


toRegex :: String -> Either String R.Regex
toRegex = (`R.compileM` []) . encodeUtf8 . pack



-- | __SPLIT(s, re)
split :: MachineState -> String -> String -> IO MachineState
split ms target regex =
  if null regex
    then returnList ms [NativeString target]
    else case toRegex regex of
           (Right r) -> returnList ms $ map NativeString $ R.split r target
           (Left s) -> throwIn ms $ InvalidRegex s



-- | __MATCH(s, re)
match :: MachineState -> String -> String -> IO MachineState
match ms target regex =
  case toRegex regex of
    (Right r) ->
      returnList ms $
      case headMay $ R.scan r target of
        Just (m, gs) -> map NativeString (m : gs)
        Nothing -> []
    (Left s) -> throwIn ms $ InvalidRegex s



-- | __MATCHES(s, re)
matches :: MachineState -> String -> String -> IO MachineState
matches ms target regex =
  case toRegex regex of
    (Right r) -> returnList ms $ map (NativeString . fst) $ R.scan r target
    (Left s) -> throwIn ms $ InvalidRegex s



-- | __JOIN(els, sep)
join :: MachineState -> Address -> String -> IO MachineState
join ms l s = do
  xs <- readStrList ms l
  return $ setCode ms (ReturnLit (NativeString $ intercalate s xs) Nothing)



-- | __LETTERS(s) - return letters of s as their own strings
letters :: MachineState -> String -> IO MachineState
letters ms s = returnList ms $ map (\c -> NativeString [c]) s


nativeToString :: Native -> String
nativeToString n =
  case n of
    NativeNumber sc ->
      case floatingOrInteger sc of
        Left f -> show (f :: Double)
        Right i -> show (i :: Integer)
    NativeString s -> s
    NativeSymbol s -> unintern s
    NativeDynamic _ -> "#DYN"


-- | __STR(n) - convert native to string in default way
strNat :: MachineState -> Native -> IO MachineState
strNat ms n =
  return $
  setCode ms $
  (`ReturnLit` Nothing) $
  NativeString $ nativeToString n



-- | __UPPER(s) - convert s to upper case
strUpper :: MachineState -> String -> IO MachineState
strUpper ms s =
  return $ setCode ms $ (`ReturnLit` Nothing) $ NativeString (map toUpper s)



-- | __LOWER(s) - convert s to lower case
strLower :: MachineState -> String -> IO MachineState
strLower ms s =
  return $ setCode ms $ (`ReturnLit` Nothing) $ NativeString (map toLower s)



-- | __FMT(obj, fmtstring)
fmt :: MachineState -> Native -> String -> IO MachineState
fmt ms obj spec =
  let result =
        case obj of
          (NativeNumber n) ->
            case floatingOrInteger n of
              (Left r) -> PF.printf spec (r :: Double)
              (Right i) -> PF.printf spec (i :: Integer)
          (NativeString s) -> PF.printf spec s
          (NativeSymbol s) -> PF.printf spec $ unintern s
          nat -> throwIn ms $ InvalidFormatSpecifier spec nat
   in return $ setCode ms (ReturnLit (NativeString result) Nothing)



-- | __SYM(s) - create symbol from string
strSym :: MachineState -> String -> IO MachineState
strSym ms nm = return $ setCode ms $ (`ReturnLit` Nothing) $ NativeSymbol $ intern nm
