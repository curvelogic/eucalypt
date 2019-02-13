{-|
Module      : Eucalypt.Stg.Intrinsics
Description : Table of intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics where

import qualified Data.Array as A
import Data.List (find, findIndex)
import Data.Maybe (fromMaybe)
import Eucalypt.Stg.IntrinsicInfo
import qualified Eucalypt.Stg.Intrinsics.Arithmetic as Arith
import qualified Eucalypt.Stg.Intrinsics.Block as Block
import qualified Eucalypt.Stg.Intrinsics.Debug as Debug
import qualified Eucalypt.Stg.Intrinsics.Emit as Emit
import qualified Eucalypt.Stg.Intrinsics.IOHMBlock as IOHM
import qualified Eucalypt.Stg.Intrinsics.IOSMBlock as IOSM
import qualified Eucalypt.Stg.Intrinsics.Meta as Meta
import qualified Eucalypt.Stg.Intrinsics.Number as Number
import qualified Eucalypt.Stg.Intrinsics.Panic as Panic
import qualified Eucalypt.Stg.Intrinsics.Str as Str
import qualified Eucalypt.Stg.Intrinsics.Time as Time
import qualified Eucalypt.Stg.Intrinsics.Eq as Eq
import qualified Eucalypt.Stg.Intrinsics.General as General
import Eucalypt.Stg.Machine

intrinsics :: [IntrinsicInfo]
intrinsics =
  concat
    [ Arith.intrinsics
    , Block.intrinsics
    , Debug.intrinsics
    , Emit.intrinsics
    , Eq.intrinsics
    , General.intrinsics
    , IOHM.intrinsics
    , IOSM.intrinsics
    , Meta.intrinsics
    , Number.intrinsics
    , Panic.intrinsics
    , Str.intrinsics
    , Time.intrinsics
    ]

-- | Used during compilation to find the index at which an intrinsic
-- will be available
intrinsicIndex :: String -> Int
intrinsicIndex n =
  fromMaybe
    (error $ "No such intrinsic: " ++ n)
    (findIndex ((n ==) . name) intrinsics)

-- | Used during compilation to find the index at which an intrinsic
-- will be available
intrinsicArity :: String -> Int
intrinsicArity n =
  maybe
    (error $ "No such intrinsic: " ++ n)
    arity
    (find ((n ==) . name) intrinsics)


-- | Intrinsics packed into an array for faster access
intrinsicArray :: A.Array Int (MachineState -> ValVec -> IO MachineState)
intrinsicArray =
  let len = length intrinsics
   in A.array (0, len) [(i, impl (intrinsics !! i)) | i <- [0 .. len]]

-- | Used during runtime to access the intrinsic implementation
intrinsicFunction :: Int -> MachineState -> ValVec -> IO MachineState
intrinsicFunction i = intrinsicArray A.! i
