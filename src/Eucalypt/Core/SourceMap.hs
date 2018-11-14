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

import Safe
import Eucalypt.Reporting.Location
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

type SMID = Int

newtype SourceMap = SourceMap { sourceMap :: IntMap SourceSpan }
  deriving (Eq, Show)

instance Semigroup SourceMap where
  (<>) (SourceMap l) (SourceMap r) = SourceMap (l <> r)

instance Monoid SourceMap where
  mempty = SourceMap mempty



-- | Next free source map ID from SourceMap
nextSMID :: SMID -> SourceMap -> SMID
nextSMID initSMID SourceMap {..} =
  maybe initSMID ((+ 1) . fst) $ IM.lookupMax sourceMap



-- | Insert new SourceSpan at next available key
addSpan :: SourceMap -> SMID -> SourceSpan -> (SMID, SourceMap)
addSpan sm@SourceMap{..} initSMID sp =
  let k = nextSMID initSMID sm
   in (k, SourceMap $ IM.insert k sp sourceMap)



-- | Retrieve source span from SourceMap id
lookupSource :: SourceMap -> SMID -> Maybe SourceSpan
lookupSource (SourceMap sm) smid = IM.lookup smid sm



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


class HasSourceMapIds a where
  toSourceMapIds :: a -> [SMID]


-- | Find source using source map and first available source map id
toSource :: HasSourceMapIds e => SourceMap -> e -> Maybe SourceSpan
toSource sm e = headMay goodIds >>= lookupSource sm
  where
    goodIds = filter (> 0) $ toSourceMapIds e
