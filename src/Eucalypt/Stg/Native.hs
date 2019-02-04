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

import Control.DeepSeq (NFData)
import Data.Foldable (toList)
import qualified Data.Map.Strict as MS
import Data.Scientific
import qualified Data.Set as S
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
  | NativeSet !(S.Set Native)
  | NativeDict !(MS.Map Native Native)
  deriving (Eq, Show, Generic, Ord)

instance NFData Native

instance StgPretty Native where
  prettify (NativeNumber i) = either P.float P.int $ floatingOrInteger i
  prettify (NativeString s) = P.text $ show s
  prettify (NativeSymbol s) = P.colon <> P.text s
  prettify (NativeSet xs) =
    P.text "#{" <> P.hcat (P.punctuate P.comma (map prettify (toList xs))) <>
    P.text "}"
  prettify (NativeDict dict) =
    P.text "#{" <>
    P.hcat
      (P.punctuate
         P.comma
         (map
            (\(k, v) -> prettify k P.<+> P.text "=>" P.<+> prettify v)
            (MS.assocs dict))) <>
    P.text "}"

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
      , NativeSet <$> arbitrary
      , NativeDict <$> arbitrary
      ]
