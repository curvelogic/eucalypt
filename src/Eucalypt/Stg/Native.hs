{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Eucalypt.Stg.Native
Description : Native types understood by the STG machine
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Native where

import Data.Dynamic
import Data.Scientific
import Eucalypt.Stg.Pretty
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(..), Gen, oneof)
import qualified Text.PrettyPrint as P

-- | Primitives that can live on the stack.
--
-- Not worried about efficiency of representation for now.
data Native
  = NativeNumber !Scientific
  | NativeString !String
  | NativeSymbol !String
  | NativeDynamic !Dynamic
  deriving (Show, Generic)

instance Eq Native where
  (==) (NativeNumber a) (NativeNumber b) = a == b
  (==) (NativeString a) (NativeString b) = a == b
  (==) (NativeSymbol a) (NativeSymbol b) = a == b
  (==) _ _ = False

instance StgPretty Native where
  prettify (NativeNumber i) = either P.float P.int $ floatingOrInteger i
  prettify (NativeString s) = P.text $ show s
  prettify (NativeSymbol s) = P.colon <> P.text s
  prettify (NativeDynamic _) = P.text "?"

instance Arbitrary Scientific where
  arbitrary =
    oneof
      [fromInteger <$> arbitrary, fromFloatDigits <$> (arbitrary :: Gen Float)]

instance Arbitrary Native where
  arbitrary =
    oneof
      [ NativeNumber <$> arbitrary
      , NativeString <$> arbitrary
      , NativeSymbol <$> arbitrary
      ]
