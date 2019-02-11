{-# LANGUAGE DeriveTraversable #-}
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
  , asSeq
  , fromSeq
  , Environments(..)
  , Reference(..)
  , values
  , value
  , module Eucalypt.Stg.Ref
  ) where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Eucalypt.Stg.Pretty
import Eucalypt.Stg.Ref
import qualified Text.PrettyPrint as P

-- | Vec of values
newtype Vec a =
  Vec (Seq.Seq a)
  deriving ( Eq
           , Show
           , Semigroup
           , Monoid
           , Functor
           , Applicative
           , Monad
           , Foldable
           , Traversable
           )

toVec :: [a] -> Vec a
toVec = Vec . Seq.fromList

fromSeq :: Seq.Seq a -> Vec a
fromSeq = Vec

envSize :: Vec a -> Int
envSize (Vec v) = Seq.length v

singleton :: a -> Vec a
singleton = Vec . Seq.singleton

index :: Vec a -> Word -> a
index (Vec xs) n = xs `Seq.index` fromIntegral n

asSeq :: Vec a -> Seq.Seq a
asSeq (Vec xs) = xs

-- | Split a Vec in two at the specified index
splitVecAt :: Integral n => n -> Vec a -> (Vec a, Vec a)
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

values :: Environments e a => e -> RefVec a -> Vec a
values e r = extractVals $ resolveVec e r

value :: Environments e a => e -> Reference a -> a
value e r = extract $ resolve e r

extractVals :: RefVec a -> Vec a
extractVals (RefVec rs) = Vec $ fmap extract rs

resolveVec :: Environments e a => e -> RefVec a -> RefVec a
resolveVec env (RefVec rs) = RefVec $ fmap (resolve env) rs

resolve :: Environments e a => e -> Reference a -> Reference a
resolve e (L n) = V $ local e `index` n
resolve e (G n) = V $ global e `index` n
resolve _ (V x) = V x
