{-# LANGUAGE RecordWildCards, OverloadedStrings, QuasiQuotes, LambdaCase #-}
{-|
Module      : Eucalypt.Stg.Intrinsics.Time
Description : Time and date intrinsics
Copyright   : (c) Greg Hawkins, 2018-2020
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Time
  ( intrinsics
  , toZonedDateTime
  ) where

import Control.Applicative
import Data.Dynamic
import Data.Scientific
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Eucalypt.Stg.Error
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common (cast, invoke, returnDynamic, returnList)
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Text.Regex.PCRE.Heavy (re, scan)

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "IFIELDS" 1 (invoke instantToUTCFields)
  , IntrinsicInfo "ZDT" 7 (invoke toZonedDateTime)
  , IntrinsicInfo "ZDT.FIELDS" 1 (invoke zdtFields)
  , IntrinsicInfo "ZDT.PARSE8601" 1 (invoke parseIso8601)
  , IntrinsicInfo "ZDT.FORMAT8601" 1 (invoke formatIso8601)
  ]


-- | Convert a POSIX timestamp into UTC calendar fields
instantToUTCFields :: MachineState -> Scientific -> IO MachineState
instantToUTCFields s d =
  let u = posixSecondsToUTCTime $ fromRational $ toRational d
      z = utcToZonedTime utc u
   in returnList s $ zonedDateTimeFields z

-- | Represent a 'ZonedTime' as its component fields
zonedDateTimeFields :: ZonedTime -> [(Native, Native)]
zonedDateTimeFields ZonedTime {..} =
  let LocalTime {..} = zonedTimeToLocalTime
      tz = timeZoneOffsetString zonedTimeZone
      (year, month, day) = toGregorian localDay
      TimeOfDay {..} = localTimeOfDay
   in [ (NativeSymbol "y", NativeNumber $ fromIntegral year)
      , (NativeSymbol "m", NativeNumber $ fromIntegral month)
      , (NativeSymbol "d", NativeNumber $ fromIntegral day)
      , (NativeSymbol "H", NativeNumber $ fromIntegral todHour)
      , (NativeSymbol "M", NativeNumber $ fromIntegral todMin)
      , (NativeSymbol "S", NativeNumber $ fromRational $ toRational todSec)
      , (NativeSymbol "Z", NativeString tz)
      ]

-- | Parse a time zone string into a 'TimeZone', supports numeric or "UTC"
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

-- | Create a raw zoned date time from its components
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

-- | Split up a raw zoned date time into its components (as k-v pairs
-- for populating a block).
zdtFields :: MachineState -> Dynamic -> IO MachineState
zdtFields ms zdtDyn = do
  zdt <- cast ms zdtDyn
  returnList ms $ zonedDateTimeFields zdt


parseZoned :: FormatExtension -> Format ZonedTime
parseZoned fe =
  let fcal = calendarFormat fe
      ftod = timeOfDayFormat fe
   in zonedTimeFormat fcal ftod fe

parseUTC :: FormatExtension -> Format UTCTime
parseUTC fe =
  let fcal = calendarFormat fe
      ftod = timeOfDayFormat fe
   in utcTimeFormat fcal ftod

parseUTCNoSeconds :: FormatExtension -> Format UTCTime
parseUTCNoSeconds fe =
  let fcal = calendarFormat fe
      ftod = hourMinuteFormat fe
   in utcTimeFormat fcal ftod

parseLocal :: FormatExtension -> Format LocalTime
parseLocal fe =
  let fcal = calendarFormat fe
      ftod = timeOfDayFormat fe
   in localTimeFormat fcal ftod

parseLocalNoSeconds :: FormatExtension -> Format LocalTime
parseLocalNoSeconds fe =
  let fcal = calendarFormat fe
      ftod = hourMinuteFormat fe
   in localTimeFormat fcal ftod

dayToZonedTime :: Day -> ZonedTime
dayToZonedTime day =
  ZonedTime
    { zonedTimeToLocalTime =
        LocalTime {localDay = day, localTimeOfDay = midnight}
    , zonedTimeZone = utc
    }

-- | Parse an ISO8601 formatted date to a native datetime
parseIso8601 :: MachineState -> String -> IO MachineState
parseIso8601 ms dateString =
  case parseFormatExtension parseZoned dateString <|>
       (utcToZonedTime utc <$> parseFormatExtension parseUTC dateString) <|>
       (utcToZonedTime utc <$> parseFormatExtension parseUTCNoSeconds dateString) <|>
       (utcToZonedTime utc . localTimeToUTC utc <$>
        parseFormatExtension parseLocal dateString) <|>
       (utcToZonedTime utc . localTimeToUTC utc <$>
        parseFormatExtension parseLocalNoSeconds dateString) <|>
       (dayToZonedTime <$> parseFormatExtension calendarFormat dateString) of
    Just zdt -> returnDynamic ms (zdt :: ZonedTime)
    _ -> throwIn ms $ InvalidArgument "Unrecognised datetime format."


-- | Format a native datetime as ISO8601
formatIso8601 :: MachineState -> Dynamic -> IO MachineState
formatIso8601 ms zdtDyn = do
  zdt <- cast ms zdtDyn
  return $ setCode ms $ (`ReturnLit` Nothing) $ NativeString (iso8601Show (zdt :: ZonedTime))
