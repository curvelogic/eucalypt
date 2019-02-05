{-|
Module      : Eucalypt.Stg.Globals.ArithmeticSpec
Description : Tests for arithmetic globals
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.ArithmeticSpec
  ( main
  , spec
  ) where

import Data.Fixed (mod')
import Data.Scientific
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.StgTestUtil
import Test.Hspec
import Test.QuickCheck
import qualified Test.QuickCheck.Monadic as QM

main :: IO ()
main = hspec spec

spec :: Spec
spec = arithSpec

addsIntegers :: Integer -> Integer -> Property
addsIntegers l r =
  QM.monadicIO $
  calculates
    (appfn_ (gref "ADD") [V $ nat l, V $ nat r])
    (returnsNative (NativeNumber $ fromInteger (l + r)))

addsFloats :: Double -> Double -> Property
addsFloats l r =
  QM.monadicIO $
  calculates
    (appfn_
       (gref "ADD")
       [ V $ NativeNumber $ fromFloatDigits l
       , V $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (fromFloatDigits l + fromFloatDigits r)))

subtractsIntegers :: Integer -> Integer -> Property
subtractsIntegers l r =
  QM.monadicIO $
  calculates
    (appfn_ (gref "SUB") [V $ nat l, V $ nat r])
    (returnsNative (NativeNumber $ fromInteger (l - r)))

subtractsFloats :: Double -> Double -> Property
subtractsFloats l r =
  QM.monadicIO $
  calculates
    (appfn_
       (gref "SUB")
       [ V $ NativeNumber $ fromFloatDigits l
       , V $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (fromFloatDigits l - fromFloatDigits r)))

multipliesIntegers :: Integer -> Integer -> Property
multipliesIntegers l r =
  QM.monadicIO $
  calculates
    (appfn_ (gref "MUL") [V $ nat l, V $ nat r])
    (returnsNative (NativeNumber $ fromInteger (l * r)))

multipliesFloats :: Double -> Double -> Property
multipliesFloats l r =
  QM.monadicIO $
  calculates
    (appfn_
       (gref "MUL")
       [ V $ NativeNumber $ fromFloatDigits l
       , V $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (fromFloatDigits l * fromFloatDigits r)))

dividesIntegers :: Integer -> NonZero Integer -> Property
dividesIntegers l (NonZero r) =
  QM.monadicIO $
  calculates
    (appfn_ (gref "DIV") [V $ nat l, V $ nat r])
    (returnsNative
       (NativeNumber $
        fromFloatDigits ((fromInteger l :: Double) / (fromInteger r :: Double))))

dividesFloats :: Double -> NonZero Double -> Property
dividesFloats l (NonZero r) =
  QM.monadicIO $
  calculates
    (appfn_
       (gref "DIV")
       [ V $ NativeNumber $ fromFloatDigits l
       , V $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (fromFloatDigits l / fromFloatDigits r)))

modulosIntegers :: Integer -> NonZero Integer -> Property
modulosIntegers l (NonZero r) =
  QM.monadicIO $
  calculates
    (appfn_ (gref "MOD") [V $ nat l, V $ nat r])
    (returnsNative
       (NativeNumber $
        fromFloatDigits (mod' (fromInteger l :: Double) (fromInteger r :: Double))))

modulosFloats :: Double -> NonZero Double -> Property
modulosFloats l (NonZero r) =
  QM.monadicIO $
  calculates
    (appfn_
       (gref "MOD")
       [ V $ NativeNumber $ fromFloatDigits l
       , V $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (mod' (fromFloatDigits l) (fromFloatDigits r))))


ordersIntegersWithLt :: Integer -> Integer -> Property
ordersIntegersWithLt l r =
  QM.monadicIO $
  calculates
    (let_
       [ pc0_ $ value_ $ appfn_ (gref "LT") [V $ nat l, V $ nat r]
       , pc0_ $
         value_ $ appfn_ (gref "GTE") [V $ nat l, V $ nat r]
       ]
       (appfn_ (gref "OR") [L 0, L 1]))
    returnsTrue

ordersIntegersWithGt :: Integer -> Integer -> Property
ordersIntegersWithGt l r =
  QM.monadicIO $
  calculates
    (let_
       [ pc0_ $ value_ $ appfn_ (gref "GT") [V $ nat l, V $ nat r]
       , pc0_ $
         value_ $ appfn_ (gref "LTE") [V $ nat l, V $ nat r]
       ]
       (appfn_ (gref "OR") [L 0, L 1]))
    returnsTrue

ordersFloatsWithLt :: Double -> Double -> Property
ordersFloatsWithLt l r =
  QM.monadicIO $
  calculates
    (let_
       [ pc0_ $
         value_ $
         appfn_
           (gref "LT")
           [ V $ NativeNumber $ fromFloatDigits l
           , V $ NativeNumber $ fromFloatDigits r
           ]
       , pc0_ $
         value_ $
         appfn_
           (gref "GTE")
           [ V $ NativeNumber $ fromFloatDigits l
           , V $ NativeNumber $ fromFloatDigits r
           ]
       ]
       (appfn_ (gref "OR") [L 0, L 1]))
    returnsTrue


ordersFloatsWithGt :: Double -> Double -> Property
ordersFloatsWithGt l r =
  QM.monadicIO $
  calculates
    (let_
       [ pc0_ $
         value_ $
         appfn_
           (gref "GT")
           [ V $ NativeNumber $ fromFloatDigits l
           , V $ NativeNumber $ fromFloatDigits r
           ]
       , pc0_ $
         value_ $
         appfn_
           (gref "LTE")
           [ V $ NativeNumber $ fromFloatDigits l
           , V $ NativeNumber $ fromFloatDigits r
           ]
       ]
       (appfn_ (gref "OR") [L 0, L 1]))
    returnsTrue

equatesEqualInts :: Integer -> Property
equatesEqualInts n =
  QM.monadicIO $
  calculates
    (appfn_ (gref "EQ") [V $ nat n, V $ nat n])
    returnsTrue

equatesEqualFloats :: Double -> Property
equatesEqualFloats n =
  QM.monadicIO $
  calculates
    (appfn_
       (gref "EQ")
       [ V $ NativeNumber $ fromFloatDigits n
       , V $ NativeNumber $ fromFloatDigits n
       ])
    returnsTrue

floors :: Double -> Property
floors d =
  QM.monadicIO $
  calculates
    (appfn_ (gref "FLOOR") [V $ NativeNumber $ fromFloatDigits d])
    (returnsNative $ NativeNumber $ fromIntegral (floor d :: Integer))

ceilings :: Double -> Property
ceilings d =
  QM.monadicIO $
  calculates
    (appfn_ (gref "CEILING") [V $ NativeNumber $ fromFloatDigits d])
    (returnsNative $ NativeNumber $ fromIntegral (ceiling d :: Integer))


arithSpec :: Spec
arithSpec =
  describe "Arithmetic operations" $ do
    it "adds ints" $ property addsIntegers
    it "subtracts ints" $ property subtractsIntegers
    it "multiplies ints" $ property multipliesIntegers
    it "adds floats" $ property addsFloats
    it "subtracts floats" $ property subtractsFloats
    it "multiplies floats" $ property multipliesFloats
    it "divides integers" $ property dividesIntegers
    xit "divides floats" $ property dividesFloats -- TODO hang in fromFloatDigits
    it "modulos integers" $ property modulosIntegers
    it "modulos floats" $ property modulosFloats
    it "orders ints with < / >=" $ property ordersIntegersWithLt
    it "orders ints with <= / >" $ property ordersIntegersWithGt
    it "orders floats with < / >=" $ property ordersFloatsWithLt
    it "orders floats with <= / >" $ property ordersFloatsWithGt
    it "equates equal ints" $ property equatesEqualInts
    it "equates equal floats" $ property equatesEqualFloats
    it "calculates floors" $ property floors
    it "calculates ceilings" $ property ceilings
