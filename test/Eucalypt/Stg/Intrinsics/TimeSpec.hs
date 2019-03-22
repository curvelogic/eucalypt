{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Intrinsics.TimeSpec
Description : Tests for time and date intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.TimeSpec
  ( main
  , spec
  ) where

import Data.Time.Calendar
import Data.Time.LocalTime
import Eucalypt.Stg.Intrinsics.Time
import Eucalypt.Stg.StgTestUtil
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "ZonedDateTime creation" $ do
    it "Respects numeric arguments" $
      (zonedTimeToLocalTime . dynamicReturn <$>
       inBlankMachine (\ms -> toZonedDateTime ms 2019 12 5 11 21 55 "UTC")) `shouldReturn`
      LocalTime (fromGregorian 2019 12 5) (TimeOfDay 11 21 55)
    it "Respects UTC as time zone argument" $
      (timeZoneName . zonedTimeZone . dynamicReturn <$>
       inBlankMachine (\ms -> toZonedDateTime ms 2019 12 5 11 21 55 "UTC")) `shouldReturn`
      "UTC"
    it "Respects +0400 as time zone argument" $
      (timeZoneMinutes . zonedTimeZone . dynamicReturn <$>
       inBlankMachine (\ms -> toZonedDateTime ms 2019 12 5 11 21 55 "+0400")) `shouldReturn`
      240
    it "Respects -0030 as time zone argument" $
      (timeZoneMinutes . zonedTimeZone . dynamicReturn <$>
       inBlankMachine (\ms -> toZonedDateTime ms 2019 12 5 11 21 55 "-0030")) `shouldReturn`
      (- 30)
