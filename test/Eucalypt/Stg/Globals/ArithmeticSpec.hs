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
    (appfn_ (Global "ADD") [Literal $ nat l, Literal $ nat r])
    (returnsNative (NativeNumber $ fromInteger (l + r)))

addsFloats :: Double -> Double -> Property
addsFloats l r =
  QM.monadicIO $
  calculates
    (appfn_
       (Global "ADD")
       [ Literal $ NativeNumber $ fromFloatDigits l
       , Literal $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (fromFloatDigits l + fromFloatDigits r)))

subtractsIntegers :: Integer -> Integer -> Property
subtractsIntegers l r =
  QM.monadicIO $
  calculates
    (appfn_ (Global "SUB") [Literal $ nat l, Literal $ nat r])
    (returnsNative (NativeNumber $ fromInteger (l - r)))

subtractsFloats :: Double -> Double -> Property
subtractsFloats l r =
  QM.monadicIO $
  calculates
    (appfn_
       (Global "SUB")
       [ Literal $ NativeNumber $ fromFloatDigits l
       , Literal $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (fromFloatDigits l - fromFloatDigits r)))

multipliesIntegers :: Integer -> Integer -> Property
multipliesIntegers l r =
  QM.monadicIO $
  calculates
    (appfn_ (Global "MUL") [Literal $ nat l, Literal $ nat r])
    (returnsNative (NativeNumber $ fromInteger (l * r)))

multipliesFloats :: Double -> Double -> Property
multipliesFloats l r =
  QM.monadicIO $
  calculates
    (appfn_
       (Global "MUL")
       [ Literal $ NativeNumber $ fromFloatDigits l
       , Literal $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (fromFloatDigits l * fromFloatDigits r)))

dividesIntegers :: Integer -> NonZero Integer -> Property
dividesIntegers l (NonZero r) =
  QM.monadicIO $
  calculates
    (appfn_ (Global "DIV") [Literal $ nat l, Literal $ nat r])
    (returnsNative
       (NativeNumber $
        fromFloatDigits ((fromInteger l :: Double) / (fromInteger r :: Double))))

dividesFloats :: Double -> NonZero Double -> Property
dividesFloats l (NonZero r) =
  QM.monadicIO $
  calculates
    (appfn_
       (Global "DIV")
       [ Literal $ NativeNumber $ fromFloatDigits l
       , Literal $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (fromFloatDigits l / fromFloatDigits r)))

modulosIntegers :: Integer -> NonZero Integer -> Property
modulosIntegers l (NonZero r) =
  QM.monadicIO $
  calculates
    (appfn_ (Global "MOD") [Literal $ nat l, Literal $ nat r])
    (returnsNative
       (NativeNumber $
        fromFloatDigits (mod' (fromInteger l :: Double) (fromInteger r :: Double))))

modulosFloats :: Double -> NonZero Double -> Property
modulosFloats l (NonZero r) =
  QM.monadicIO $
  calculates
    (appfn_
       (Global "MOD")
       [ Literal $ NativeNumber $ fromFloatDigits l
       , Literal $ NativeNumber $ fromFloatDigits r
       ])
    (returnsNative (NativeNumber (mod' (fromFloatDigits l) (fromFloatDigits r))))


ordersIntegersWithLt :: Integer -> Integer -> Property
ordersIntegersWithLt l r =
  QM.monadicIO $
  calculates
    (let_
       [ pc0_ $ value_ $ appfn_ (Global "LT") [Literal $ nat l, Literal $ nat r]
       , pc0_ $
         value_ $ appfn_ (Global "GTE") [Literal $ nat l, Literal $ nat r]
       ]
       (appfn_ (Global "OR") [Local 0, Local 1]))
    returnsTrue

ordersIntegersWithGt :: Integer -> Integer -> Property
ordersIntegersWithGt l r =
  QM.monadicIO $
  calculates
    (let_
       [ pc0_ $ value_ $ appfn_ (Global "GT") [Literal $ nat l, Literal $ nat r]
       , pc0_ $
         value_ $ appfn_ (Global "LTE") [Literal $ nat l, Literal $ nat r]
       ]
       (appfn_ (Global "OR") [Local 0, Local 1]))
    returnsTrue

ordersFloatsWithLt :: Double -> Double -> Property
ordersFloatsWithLt l r =
  QM.monadicIO $
  calculates
    (let_
       [ pc0_ $
         value_ $
         appfn_
           (Global "LT")
           [ Literal $ NativeNumber $ fromFloatDigits l
           , Literal $ NativeNumber $ fromFloatDigits r
           ]
       , pc0_ $
         value_ $
         appfn_
           (Global "GTE")
           [ Literal $ NativeNumber $ fromFloatDigits l
           , Literal $ NativeNumber $ fromFloatDigits r
           ]
       ]
       (appfn_ (Global "OR") [Local 0, Local 1]))
    returnsTrue


ordersFloatsWithGt :: Double -> Double -> Property
ordersFloatsWithGt l r =
  QM.monadicIO $
  calculates
    (let_
       [ pc0_ $
         value_ $
         appfn_
           (Global "GT")
           [ Literal $ NativeNumber $ fromFloatDigits l
           , Literal $ NativeNumber $ fromFloatDigits r
           ]
       , pc0_ $
         value_ $
         appfn_
           (Global "LTE")
           [ Literal $ NativeNumber $ fromFloatDigits l
           , Literal $ NativeNumber $ fromFloatDigits r
           ]
       ]
       (appfn_ (Global "OR") [Local 0, Local 1]))
    returnsTrue

equatesEqualInts :: Integer -> Property
equatesEqualInts n =
  QM.monadicIO $
  calculates
    (appfn_ (Global "EQ") [Literal $ nat n, Literal $ nat n])
    returnsTrue

equatesEqualFloats :: Double -> Property
equatesEqualFloats n =
  QM.monadicIO $
  calculates
    (appfn_
       (Global "EQ")
       [ Literal $ NativeNumber $ fromFloatDigits n
       , Literal $ NativeNumber $ fromFloatDigits n
       ])
    returnsTrue

floors :: Double -> Property
floors d =
  QM.monadicIO $
  calculates
    (appfn_ (Global "FLOOR") [Literal $ NativeNumber $ fromFloatDigits d])
    (returnsNative $ NativeNumber $ fromIntegral (floor d :: Integer))

ceilings :: Double -> Property
ceilings d =
  QM.monadicIO $
  calculates
    (appfn_ (Global "CEILING") [Literal $ NativeNumber $ fromFloatDigits d])
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
