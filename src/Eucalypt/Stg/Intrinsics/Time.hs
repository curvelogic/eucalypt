{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Intrinsics.Time
Description : Time and date intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Time (intrinsics) where

import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common (invoke, returnNatPairList)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Machine
import Data.Scientific
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "IFIELDS" 1 (invoke instantToUTCFields)
  ]


-- | Convert a POSIX timestamp into UTC calendar fields
instantToUTCFields :: MachineState -> Scientific -> IO MachineState
instantToUTCFields s d =
  let u = posixSecondsToUTCTime $ fromRational $ toRational d
      z = utcToZonedTime utc u
   in returnNatPairList s $ zonedDateTimeFields z

zonedDateTimeFields :: ZonedTime -> [(Native, Native)]
zonedDateTimeFields ZonedTime {..} =
  let LocalTime {..} = zonedTimeToLocalTime
      tz = timeZoneOffsetString zonedTimeZone
      (year, month, day) = toGregorian localDay
      TimeOfDay {..} = localTimeOfDay
   in [ (NativeSymbol "y", NativeNumber $ fromIntegral year)
      , (NativeSymbol "M", NativeNumber $ fromIntegral month)
      , (NativeSymbol "d", NativeNumber $ fromIntegral day)
      , (NativeSymbol "h", NativeNumber $ fromIntegral todHour)
      , (NativeSymbol "m", NativeNumber $ fromIntegral todMin)
      , (NativeSymbol "s", NativeNumber $ fromRational $ toRational todSec)
      , (NativeSymbol "Z", NativeString tz)
      ]
