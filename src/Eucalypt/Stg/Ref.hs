{-# LANGUAGE ViewPatterns #-}
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

module Eucalypt.Stg.Ref where

import Control.Arrow ((***))
import Control.Comonad
import Eucalypt.Stg.Pretty
import qualified Text.PrettyPrint as P
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

-- | Reference a is index into Vec a or an a override
data Reference a = L !Word | G !Word | V !a
  deriving (Eq, Show)

instance Functor Reference where
  fmap f (V x) = V (f x)
  fmap _ (L n) = L n
  fmap _ (G n) = G n

instance Comonad Reference where
  extract (V a) = a
  extract _ = error "Non-value ref"
  extend f r = V $ f r

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

refIndex :: RefVec a -> Int -> Reference a
refIndex (RefVec rs) i = rs `Seq.index` i

extract :: Reference a -> a
extract (V a) = a
extract _ = error "Non-value ref"

range :: Integral n => n -> n -> RefVec a
range from to =
  let b = fromIntegral from :: Int
      e = fromIntegral to :: Int
   in refs $ map (L . fromIntegral) [b .. (e - 1)]

headAndTail :: RefVec a -> (Reference a, RefVec a)
headAndTail (RefVec (Seq.viewl -> h Seq.:< t)) = (h, RefVec t)
headAndTail _ = error "Illegal decomposition of empty sequence"

-- | When we have a set of references to use and are generating a
-- PreClosure, we declare local environment references in the
-- PreClosure but use globals and natives without declaration.
--
-- sortRefs produces two lists:
-- * the subset of refs that need declaring
-- * references to use inside the closure corresponding to the
-- references as would be used outside
sortRefs :: [Reference a] -> ([Reference a], [Reference a])
sortRefs rs = reverse *** reverse $ sortRefs' rs ([], []) 0
  where
    sortRefs' ::
         [Reference a]
      -> ([Reference a], [Reference a])
      -> Word
      -> ([Reference a], [Reference a])
    sortRefs' [] (outRefs, inRefs) _ = (outRefs, inRefs)
    sortRefs' (V n:rest) (outRefs, inRefs) i =
      sortRefs' rest (outRefs, V n : inRefs) i
    sortRefs' (G n:rest) (outRefs, inRefs) i =
      sortRefs' rest (outRefs, G n : inRefs) i
    sortRefs' (L n:rest) (outRefs, inRefs) i =
      sortRefs' rest (L n : outRefs, L i : inRefs) (i + 1)
