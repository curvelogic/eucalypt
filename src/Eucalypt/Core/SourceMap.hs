{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Core.SourceMap
Description : Tracing core AST and runtime events back to source locations
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Core.SourceMap where

import Eucalypt.Reporting.Location
import Data.IntMap

type SMID = Int

newtype SourceMap = SourceMap { sourceMap :: IntMap SourceSpan }
  deriving (Eq, Show)

instance Semigroup SourceMap where
  (<>) (SourceMap l) (SourceMap r) = SourceMap (l <> r)

instance Monoid SourceMap where
  mempty = SourceMap mempty

-- | Insert new SourceSpan at next available key
addSpan :: SourceMap -> SourceSpan -> (SMID, SourceMap)
addSpan SourceMap{..} sp =
  let k = (maybe 1 ((+1) . fst) $ lookupMax sourceMap)
   in (k, SourceMap $ insert k sp sourceMap)



class Monad m => MonadSupplySMID m where
  recordSpan :: SourceSpan -> m SMID



-- | Turn single arg CoreExp smart constructor into monadic action
-- that mints new SMID and stores 'SourceSpan'
mint :: MonadSupplySMID m
  => (SMID -> b -> r)
  -> SourceSpan -> b -> m r
mint con sp x = (`con` x) <$> recordSpan sp



-- | Turn two arg CoreExp smart constructor into monadic action that
-- mints new SMID and stores 'SourceSpan'
mint2 :: MonadSupplySMID m
  => (SMID -> b -> c -> r)
  -> (SourceSpan -> b -> c -> m r)
mint2 con sp x y = recordSpan sp >>= \smid -> return $ con smid x y



-- | Turn two arg CoreExp smart constructor into monadic action that
-- mints new SMID and stores 'SourceSpan'
mint3 :: MonadSupplySMID m
  => (SMID -> b -> c -> d -> r)
  -> (SourceSpan -> b -> c -> d -> m r)
mint3 con sp x y z = recordSpan sp >>= \smid -> return $ con smid x y z



-- | A version of smart constructor that
anon :: (SMID -> a) -> a
anon f = f 0
