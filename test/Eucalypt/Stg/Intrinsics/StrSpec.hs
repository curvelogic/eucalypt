{-|
Module      : Eucalypt.Stg.Intrinsics.StrSpec
Description : Tests for string intrinsics
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.StrSpec
  ( main
  , spec
  ) where

import Data.Char (toUpper, toLower)
import Data.List (intercalate, isInfixOf)
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Compiler
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Intrinsics
import Eucalypt.Stg.StgTestUtil
import Test.QuickCheck
import Test.QuickCheck.Arbitrary (arbitraryPrintableChar)
import qualified Test.QuickCheck.Monadic as QM
import Test.Hspec


newtype RegexSafeString = RegexSafeString
  { getRegexSafeString :: String
  } deriving (Show, Eq)

instance Arbitrary RegexSafeString where
  arbitrary = RegexSafeString `fmap` listOf1 arbitraryNonRegexChar
    where
      arbitraryNonRegexChar =
        arbitraryPrintableChar `suchThat` (not . (`elem` "[]().\\*+{}?^\0$&|"))
  shrink (RegexSafeString xs) = RegexSafeString `fmap` shrink xs


main :: IO ()
main = hspec spec

joinStg :: [String] -> String -> StgSyn
joinStg xs sep =
  letrec_
    [pc0_ $ thunk_ $ list_ 0 (map (Literal . NativeString) xs) Nothing]
    (force_
       (appfn_ (Global "seqNatList") [Local 0])
       (appbif_ (intrinsicIndex "JOIN") [Local 1, Literal (NativeString sep)]))

joins :: [String] -> String -> Property
joins xs sep =
  QM.monadicIO $
  calculates (joinStg xs sep) $
  returnsNative (NativeString (intercalate sep xs))

splitStg :: [String] -> String -> StgSyn
splitStg xs sep =
  appbif_
    (intrinsicIndex "SPLIT")
    [Literal (NativeString joined), Literal (NativeString sep)]
  where
    joined = intercalate sep xs

splits :: NonEmptyList RegexSafeString -> RegexSafeString -> Property
splits (NonEmpty xs) (RegexSafeString sep) =
  not (any ((sep `isInfixOf`) . getRegexSafeString) xs) ==> QM.monadicIO $ do
    let components = map getRegexSafeString xs
    calculatesM (splitStg components sep)
      (fmap (components ==) . readStrListReturn)

stg1 :: String -> String -> StgSyn
stg1 bif s = appbif_ (intrinsicIndex bif) [Literal $ NativeString s]

letterStg :: String -> StgSyn
letterStg = stg1 "LETTERS"

lettery :: String -> Property
lettery s =
  QM.monadicIO $ do
    let components = map (:[]) s
    calculatesM (letterStg s) (fmap (components ==) . readStrListReturn)

upperStg :: String -> StgSyn
upperStg = stg1 "UPPER"

uppers :: String -> Property
uppers s =
  QM.monadicIO $
  calculates (upperStg s) $ returnsNative (NativeString (map toUpper s))

lowerStg :: String -> StgSyn
lowerStg = stg1 "LOWER"

lowers :: String -> Property
lowers s =
  QM.monadicIO $
  calculates (lowerStg s) $ returnsNative (NativeString (map toLower s))

spec :: Spec
spec =
  describe "string intrinsics" $ do
    it "join strings" $ property joins
    it "split strings" $ property splits
    it "extracts letters" $ property lettery
    it "upcases" $ property uppers
    it "downcases" $ property lowers
