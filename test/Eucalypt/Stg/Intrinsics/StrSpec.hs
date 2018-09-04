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
    [pc0_ $ thunk_ $ list_ 0 (map (Literal . NativeString) xs)]
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

spec :: Spec
spec =
  describe "string intrinsics" $ do
    it "joins strings" $ property joins
    it "splits strings" $ property splits
