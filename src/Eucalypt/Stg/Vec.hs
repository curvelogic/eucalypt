{-# LANGUAGE ViewPatterns #-}
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
  , RefVec
  , refCount
  , refs
  , values
  , value
  , range
  , headAndTail
  ) where

import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Eucalypt.Stg.Pretty
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


-- | Reference a is index into Vec a or an a override
data Reference a = L !Word | G !Word | V !a
  deriving (Eq, Show)

instance Functor Reference where
  fmap f (V x) = V (f x)
  fmap _ (L n) = L n
  fmap _ (G n) = G n

instance StgPretty a => StgPretty (Reference a) where
  prettify (G n) = P.char 'G' <> P.brackets (P.int $ fromIntegral n)
  prettify (L n) = P.char 'E' <> P.brackets (P.int $ fromIntegral n)
  prettify (V x) = prettify x

newtype RefVec a =
  RefVec (Seq.Seq (Reference a))
  deriving (Eq, Show, Semigroup, Monoid)

instance Functor RefVec where
  fmap f (RefVec rs) = RefVec $ fmap (fmap f) rs

instance StgPretty a => StgPretty (RefVec a) where
  prettify (RefVec rs) =
    P.hcat $ P.punctuate P.space (map prettify (toList rs))

refCount :: RefVec a -> Int
refCount (RefVec xs) = length xs

refs :: [Reference a] -> RefVec a
refs rs = RefVec $ Seq.fromList rs

values :: Environments e a => e -> RefVec a -> Vec a
values e r = extractVals $ resolveVec e r

value :: Environments e a => e -> Reference a -> a
value e r = extract $ resolve e r

extract :: Reference a -> a
extract (V a) = a
extract _ = error "Non-value ref"

extractVals :: RefVec a -> Vec a
extractVals (RefVec rs) = Vec $ fmap extract rs

resolveVec :: Environments e a => e -> RefVec a -> RefVec a
resolveVec env (RefVec rs) = RefVec $ fmap (resolve env) rs

resolve :: Environments e a => e -> Reference a -> Reference a
resolve e (L n) = V $ local e `index` n
resolve e (G n) = V $ global e `index` n
resolve _ (V x) = V x

range :: Integral n => n -> n -> RefVec a
range from to =
  let b = fromIntegral from :: Int
      e = fromIntegral to :: Int
   in refs $ map (L . fromIntegral) [b .. (e - 1)]

headAndTail :: RefVec a -> (Reference a, RefVec a)
headAndTail (RefVec (Seq.viewl -> h Seq.:< t)) = (h, RefVec t)
headAndTail _ = error "Illegal decomposition of empty sequence"
