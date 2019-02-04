{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-|
Module      : Eucalypt.Stg.Vec
Description : Sequence structure for refs and values in the STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Vec
  ( Vec
  , toVec
  , envSize
  , singleton
  , index
  , splitVecAt
  , Environments(..)
  , Ref(..)
  , RefVec
  , refs
  , vals
  , range
  ) where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Eucalypt.Stg.Pretty
import qualified Text.PrettyPrint as P

-- | Vec of values
newtype Vec a =
  Vec (Seq.Seq a)
  deriving (Eq, Show, Semigroup, Monoid)

toVec :: [a] -> Vec a
toVec = Vec . Seq.fromList

envSize :: Vec a -> Int
envSize (Vec v) = Seq.length v

singleton :: a -> Vec a
singleton = Vec . Seq.singleton

index :: Vec a -> Word -> a
index (Vec xs) n = xs `Seq.index` fromIntegral n

-- | Split a Vec in two at the specified index
splitVecAt :: Integral a => a -> Vec a -> (Vec a, Vec a)
splitVecAt n (Vec vs) =
  let (l, r) = Seq.splitAt (fromIntegral n) vs
   in (Vec l, Vec r)

instance StgPretty a => StgPretty (Vec a) where
  prettify (Vec vs) = P.braces $ P.hcat $ toList $ fmap prettify vs

class Environments e a where
  local :: e -> Vec a
  global :: e -> Vec a

instance Environments (Vec a, Vec a) a where
  local (l, _) = l
  global (_, g) = g


-- | Ref a is index into Vec a or an a override
data Ref a = L !Word | G !Word | V !a
  deriving (Eq, Show)

instance Functor Ref where
  fmap f (V x) = V (f x)
  fmap _ (L n) = L n
  fmap _ (G n) = G n

newtype RefVec a =
  RefVec (Seq.Seq (Ref a))
  deriving (Eq, Show, Semigroup, Monoid)

instance Functor RefVec where
  fmap f (RefVec rs) = RefVec $ fmap (fmap f) rs

refs :: [Ref a] -> RefVec a
refs rs = RefVec $ Seq.fromList rs

vals :: Environments e a => e -> RefVec a -> Vec a
vals e r = extractVals $ resolve e r

extractVals :: RefVec a -> Vec a
extractVals (RefVec rs) = Vec $ fmap extract rs
  where
    extract (V a) = a
    extract _ = error "Non-value ref"

resolve :: Environments e a => e -> RefVec a -> RefVec a
resolve env (RefVec rs) = RefVec $ fmap (val env) rs
  where
    val e (L n) = V $ local e `index` n
    val e (G n) = V $ global e `index` n
    val _ (V x) = V x

range :: Integral n => n -> n -> RefVec a
range from to =
  RefVec $ Seq.fromList $ map L [(fromIntegral from) .. (fromIntegral to - 1)]
