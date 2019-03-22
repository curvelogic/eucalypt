{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes #-}
{-|
Module      : Eucalypt.Stg.Intrinsics.Time
Description : Time and date intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Time
  ( intrinsics
  , toZonedDateTime
  ) where

import Eucalypt.Stg.Error
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common (invoke, returnList, returnDynamic)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Machine
import Data.Scientific
import Data.Time.Calendar
import Data.Time.Clock.POSIX
import Data.Time.LocalTime
import Text.Regex.PCRE.Heavy (scan, re)

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "IFIELDS" 1 (invoke instantToUTCFields)
  , IntrinsicInfo "ZDT" 7 (invoke toZonedDateTime)
  ]


-- | Convert a POSIX timestamp into UTC calendar fields
instantToUTCFields :: MachineState -> Scientific -> IO MachineState
instantToUTCFields s d =
  let u = posixSecondsToUTCTime $ fromRational $ toRational d
      z = utcToZonedTime utc u
   in returnList s $ zonedDateTimeFields z

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

timeZoneFromString :: MachineState -> String -> IO TimeZone
timeZoneFromString _ "UTC" = return utc
timeZoneFromString _ "" = return utc
timeZoneFromString ms s =
  case scan [re|([+-])?(\d\d)(\d\d)|] s of
    [(_, [sgn, hours, mins])] ->
      let m = read mins :: Int
          h = read hours :: Int
          sig =
            if sgn == "-"
              then (-1)
              else 1
       in return $ minutesToTimeZone $ sig * (60 * h + m)
    _ -> throwIn ms $ InvalidArgument "Time zone was not valid"

toZonedDateTime ::
     MachineState
  -> Scientific
  -> Scientific
  -> Scientific
  -> Scientific
  -> Scientific
  -> Scientific
  -> String
  -> IO MachineState
toZonedDateTime ms y m d h mins s tz =
  case traverse toBoundedInteger [y, m, d, h, mins, s] of
    Just [y', m', d', h', mins', s'] -> do
      let day = fromGregorian (fromIntegral y') m' d'
      let time = TimeOfDay h' mins' (fromIntegral s')
      let locTime = LocalTime day time
      dtz <- timeZoneFromString ms tz
      returnDynamic ms $ ZonedTime locTime dtz
    _ -> throwIn ms $ InvalidArgument "Time field was invalid"
