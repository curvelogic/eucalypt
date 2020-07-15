{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | Largely copied from InsOrdHashMap
module Eucalypt.Stg.Intrinsics.SymbolMap where

import Prelude hiding (filter, foldr, lookup, map, null)
import Control.Applicative (Const(..), (<**>))
import Control.Arrow (first, second)
import Control.Monad (join, liftM2)
import Data.Data (Data, Typeable)
import qualified Data.Foldable as F
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Symbol
import Text.Read
import Control.Lens (_1, _2)

type SymbolMap v = Map Symbol v

-------------------------------------------------------------------------------
-- Strict Pair Int a
-------------------------------------------------------------------------------

data P a = P !Int !a
    deriving (Functor, Foldable, Traversable, Typeable, Data)

getPK :: P a -> Int
getPK (P i _) = i
{-# INLINABLE getPK #-}

getPV :: P a -> a
getPV (P _ a) = a
{-# INLINABLE getPV #-}

incPK :: Int -> P a -> P a
incPK i (P j x) = P (i + j) x
{-# INLINABLE incPK #-}

instance Eq a => Eq (P a) where
  P _ a == P _ b = a == b

instance Ord a => Ord (P a) where
  P _ a <= P _ b = a <= b

instance Show a => Show (P a) where
  showsPrec d (P _ x) = showsPrec d x

-------------------------------------------------------------------------------
-- InsOrdMap
-------------------------------------------------------------------------------

-- | 'Map' which tries it's best to remember insertion order of elements.

data InsOrdSymbolMap v = InsOrdSymbolMap
    { _getIndex        :: !Int
    , getInsOrdSymbolMap :: !(Map Symbol (P v))
    }
    deriving (Functor, Typeable, Data)

instance Eq v => Eq (InsOrdSymbolMap v) where
    InsOrdSymbolMap _ a == InsOrdSymbolMap _ b = a == b

instance Show v => Show (InsOrdSymbolMap v) where
    showsPrec d m = showParen (d > 10) $
        showString "fromList " . showsPrec 11 (toList m)

instance Read v => Read (InsOrdSymbolMap v) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      fromList <$> readPrec

    readListPrec = readListPrecDefault

instance Semigroup (InsOrdSymbolMap v) where
    (<>) = union

instance Monoid (InsOrdSymbolMap v) where
    mempty = empty
    mappend = union

instance Foldable InsOrdSymbolMap where
    foldMap f = foldMap (f . snd) . toList
    null = null
    toList = elems
    length = size

instance Traversable InsOrdSymbolMap where
  traverse f = traverseWithKey (const f)

-------------------------------------------------------------------------------
-- Construction
-------------------------------------------------------------------------------

empty :: InsOrdSymbolMap v
empty = InsOrdSymbolMap 0 Map.empty
{-# INLINABLE empty #-}

singleton :: Symbol -> v -> InsOrdSymbolMap v
singleton k v = InsOrdSymbolMap 1 (Map.singleton k (P 0 v))
{-# INLINABLE singleton #-}

-------------------------------------------------------------------------------
-- Basic interface
-------------------------------------------------------------------------------

null :: InsOrdSymbolMap v -> Bool
null = Map.null . getInsOrdSymbolMap
{-# INLINABLE null #-}

size :: InsOrdSymbolMap v -> Int
size = Map.size . getInsOrdSymbolMap
{-# INLINABLE size #-}

member :: Symbol -> InsOrdSymbolMap a -> Bool
member k = Map.member k . getInsOrdSymbolMap
{-# INLINABLE member #-}

lookup :: Symbol -> InsOrdSymbolMap v -> Maybe v
lookup k = fmap getPV . Map.lookup k . getInsOrdSymbolMap
{-# INLINABLE lookup #-}

lookupDefault ::
     v -- ^ Default value to return.
  -> Symbol
  -> InsOrdSymbolMap v
  -> v
lookupDefault def k m = fromMaybe def $ lookup k m
{-# INLINABLE lookupDefault #-}

delete :: Symbol -> InsOrdSymbolMap v -> InsOrdSymbolMap v
delete k (InsOrdSymbolMap i m) = InsOrdSymbolMap i $ Map.delete k m
{-# INLINABLE delete #-}

insert :: Symbol -> v -> InsOrdSymbolMap v -> InsOrdSymbolMap v
insert = insertWith const
{-# INLINABLE insert #-}

insertWith :: (v -> v -> v) -> Symbol -> v -> InsOrdSymbolMap v -> InsOrdSymbolMap v
insertWith f k v = alter (Just . maybe v (f v)) k
{-# INLINABLE insertWith #-}

adjust :: (v -> v) -> Symbol -> InsOrdSymbolMap v -> InsOrdSymbolMap v
adjust f = alter (fmap f)
{-# INLINABLE adjust #-}

update
    :: (a -> Maybe a) -> Symbol -> InsOrdSymbolMap a -> InsOrdSymbolMap a
update f = alter (>>= f)
{-# INLINABLE update #-}

alter
    :: (Maybe v -> Maybe v) -> Symbol -> InsOrdSymbolMap v -> InsOrdSymbolMap v
alter f k insm@(InsOrdSymbolMap j m) =
    case Map.lookup k m of
        Nothing       -> case f Nothing of
            Nothing   -> insm
            Just v    -> InsOrdSymbolMap (j + 1) (Map.insert k (P j v) m)
        Just (P i v)  -> case f (Just v) of
            Nothing   -> InsOrdSymbolMap j (Map.delete k m)
            Just u    -> InsOrdSymbolMap j (Map.insert k (P i u) m)
{-# INLINABLE alter #-}

-------------------------------------------------------------------------------
-- Monadic versions
-------------------------------------------------------------------------------

insertM :: Monad m => Symbol -> v -> InsOrdSymbolMap v -> m (InsOrdSymbolMap v)
insertM k v m =
  let mM = map return m
   in sequence $ insert k (return v) mM

-- A monadic version of insertWith
insertWithM ::
     Monad m
  => (v -> v -> m v)
  -> Symbol
  -> v
  -> InsOrdSymbolMap v
  -> m (InsOrdSymbolMap v)
insertWithM f k v m =
  let mM = map return m
   in sequence $ insertWith fM k (return v) mM
  where
    fM x y = join $ liftM2 f x y

-- | A monadic version of unionWith
unionWithM ::
     Monad m
  => InsOrdSymbolMap v
  -> InsOrdSymbolMap v
  -> (v -> v -> m v)
  -> m (InsOrdSymbolMap v)
unionWithM l r f =
  let lM = map return l
      rM = map return r
   in sequence $ unionWith fM lM rM
  where
    fM x y = join $ liftM2 f x y


-------------------------------------------------------------------------------
-- Combine
-------------------------------------------------------------------------------

-- | The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the result.
--
-- Ordered traversal will go thru keys in the first map first.
unionWith :: (v -> v -> v) -> InsOrdSymbolMap v -> InsOrdSymbolMap v -> InsOrdSymbolMap v
unionWith f (InsOrdSymbolMap i a) (InsOrdSymbolMap j b) =
    mk $ Map.unionWith f' a b'
  where
    -- the threshold is arbitrary, it meant to amortise need for packing of indices
    mk | i > 0xfffff || j >= 0xfffff = fromMapP
       | otherwise                   = InsOrdSymbolMap (i + j)
    b' = fmap (incPK i) b
    f' (P ii x) (P _ y) = P ii (f x y)

unionWithKey ::
     (Symbol -> v -> v -> v) -> InsOrdSymbolMap v -> InsOrdSymbolMap v -> InsOrdSymbolMap v
unionWithKey f (InsOrdSymbolMap i a) (InsOrdSymbolMap j b) =
    InsOrdSymbolMap (i + j) $ Map.unionWithKey f' a b'
  where
    b' = fmap (incPK i) b
    f' k (P ii x) (P _ y) = P ii (f k x y)

union :: InsOrdSymbolMap v -> InsOrdSymbolMap v -> InsOrdSymbolMap v
union = unionWith const

unions :: (Foldable f) => f (InsOrdSymbolMap v) -> InsOrdSymbolMap v
unions = F.foldl' union empty

-------------------------------------------------------------------------------
-- Transformations
-------------------------------------------------------------------------------

-- | Order preserving mapping of keys.
mapKeys :: (Symbol -> Symbol) -> InsOrdSymbolMap v -> InsOrdSymbolMap v
mapKeys f (InsOrdSymbolMap i m) = InsOrdSymbolMap i $
    Map.fromList . fmap (first f) . Map.toList $ m

traverseKeys
    :: (Applicative f)
    => (Symbol -> f Symbol) -> InsOrdSymbolMap v -> f (InsOrdSymbolMap v)
traverseKeys f (InsOrdSymbolMap i m) = InsOrdSymbolMap i . Map.fromList <$>
    (traverse . _1) f (Map.toList m)

map :: (v1 -> v2) -> InsOrdSymbolMap v1 -> InsOrdSymbolMap v2
map = fmap

mapWithKey :: (Symbol -> v1 -> v2) -> InsOrdSymbolMap v1 -> InsOrdSymbolMap v2
mapWithKey f (InsOrdSymbolMap i m) = InsOrdSymbolMap i $ Map.mapWithKey f' m
  where
    f' k (P j x) = P j (f k x)

foldMapWithKey :: Monoid m => (Symbol -> a -> m) -> InsOrdSymbolMap a -> m
foldMapWithKey f = foldMap (uncurry f) . toList

traverseWithKey ::
     Applicative f => (Symbol -> a -> f b) -> InsOrdSymbolMap a -> f (InsOrdSymbolMap b)
traverseWithKey f (InsOrdSymbolMap n m) = InsOrdSymbolMap n <$> retractSortedAp
    (Map.traverseWithKey (\k (P i v) -> liftSortedAp i (P i <$> f k v)) m)

-- Sort using insertion sort
-- Hopefully it's fast enough for where we need it
-- otherwise: https://gist.github.com/treeowl/9621f58d55fe0c4f9162be0e074b1b29
-- http://elvishjerricco.github.io/2017/03/23/applicative-sorting.html also related

-- Free applicative which re-orders effects
-- Mostly from Edward Kmett's `free` package.
data SortedAp f a where
    Pure :: a -> SortedAp f a
    SortedAp   :: !Int -> f a -> SortedAp f (a -> b) -> SortedAp f b

instance Functor (SortedAp f) where
    fmap f (Pure a)   = Pure (f a)
    fmap f (SortedAp i x y)   = SortedAp i x ((f .) <$> y)

instance Applicative (SortedAp f) where
    pure = Pure
    Pure f <*> y = fmap f y
    -- This is different from real Ap
    f <*> Pure y = fmap ($ y) f
    f@(SortedAp i x y) <*> z@(SortedAp j u v)
        | i < j     = SortedAp i x (flip <$> y <*> z)
        | otherwise = SortedAp j u ((.) <$> f <*> v)

liftSortedAp :: Int -> f a -> SortedAp f a
liftSortedAp i x = SortedAp i x (Pure id)

retractSortedAp :: Applicative f => SortedAp f a -> f a
retractSortedAp (Pure x) = pure x
retractSortedAp (SortedAp _ f x) = f <**> retractSortedAp x

-------------------------------------------------------------------------------
-- Unordered
-------------------------------------------------------------------------------

-- | More efficient than 'foldMap', when folding in insertion order is not important.
unorderedFoldMap :: Monoid m => (a -> m) -> InsOrdSymbolMap a -> m
unorderedFoldMap f (InsOrdSymbolMap _ m) = foldMap (f . getPV) m

-- | More efficient than 'foldMapWithKey', when folding in insertion order is not important.
unorderedFoldMapWithKey :: Monoid m => (Symbol -> a -> m) -> InsOrdSymbolMap a -> m
unorderedFoldMapWithKey f m =
    getConst (unorderedTraverseWithKey (\k a -> Const (f k a)) m)

-- | More efficient than 'traverse', when traversing in insertion order is not important.
unorderedTraverse :: Applicative f => (a -> f b) -> InsOrdSymbolMap a -> f (InsOrdSymbolMap b)
unorderedTraverse f (InsOrdSymbolMap i m) =
    InsOrdSymbolMap i <$> (traverse . traverse) f m

-- | More efficient than `traverseWithKey`, when traversing in insertion order is not important.
unorderedTraverseWithKey :: Applicative f => (Symbol -> a -> f b) -> InsOrdSymbolMap a -> f (InsOrdSymbolMap b)
unorderedTraverseWithKey f (InsOrdSymbolMap i m) =
    InsOrdSymbolMap i <$> Map.traverseWithKey f' m
  where
    f' k (P j x) = P j <$> f k x

-------------------------------------------------------------------------------
-- Difference and intersection
-------------------------------------------------------------------------------

difference :: InsOrdSymbolMap v -> InsOrdSymbolMap w -> InsOrdSymbolMap v
difference (InsOrdSymbolMap i a) (InsOrdSymbolMap _ b) =
    InsOrdSymbolMap i $ Map.difference a b

intersection :: InsOrdSymbolMap v -> InsOrdSymbolMap w -> InsOrdSymbolMap v
intersection = intersectionWith const

intersectionWith ::
     (v1 -> v2 -> v3) -> InsOrdSymbolMap v1 -> InsOrdSymbolMap v2 -> InsOrdSymbolMap v3
intersectionWith f = intersectionWithKey (const f)

intersectionWithKey ::
     (Symbol -> v1 -> v2 -> v3) -> InsOrdSymbolMap v1 -> InsOrdSymbolMap v2 -> InsOrdSymbolMap v3
intersectionWithKey f (InsOrdSymbolMap i a) (InsOrdSymbolMap _ b) =
    InsOrdSymbolMap i $ Map.intersectionWithKey f' a b
  where
    f' k (P j x) (P _ y) = P j (f k x y)

-------------------------------------------------------------------------------
-- Folds
-------------------------------------------------------------------------------

foldl' :: (a -> v -> a) -> a -> InsOrdSymbolMap v -> a
foldl' f x = F.foldl' f' x . toList
  where
    f' a (_, v) = f a v

foldlWithKey' :: (a -> Symbol -> v -> a) -> a -> InsOrdSymbolMap v -> a
foldlWithKey' f x = F.foldl' f' x . toList
  where
    f' a (k, v) = f a k v

foldr :: (v -> a -> a) -> a -> InsOrdSymbolMap v -> a
foldr f x = F.foldr f' x . toList
  where
    f' (_, v) = f v

foldrWithKey :: (Symbol -> v -> a -> a) -> a -> InsOrdSymbolMap v -> a
foldrWithKey f x = F.foldr f' x . toList
  where
    f' (k, v) = f k v

-------------------------------------------------------------------------------
-- Filter
-------------------------------------------------------------------------------

filter :: (v -> Bool) -> InsOrdSymbolMap v -> InsOrdSymbolMap v
filter f (InsOrdSymbolMap i m) =
    InsOrdSymbolMap i $ Map.filter (f . getPV) m

filterWithKey :: (Symbol -> v -> Bool) -> InsOrdSymbolMap v -> InsOrdSymbolMap v
filterWithKey f (InsOrdSymbolMap i m) =
    InsOrdSymbolMap i $ Map.filterWithKey f' m
  where
    f' k (P _ x) = f k x

mapMaybe :: (v1 -> Maybe v2) -> InsOrdSymbolMap v1 -> InsOrdSymbolMap v2
mapMaybe f (InsOrdSymbolMap i m) = InsOrdSymbolMap i $ Map.mapMaybe f' m
  where
    f' (P j x) = P j <$> f x

mapMaybeWithKey :: (Symbol -> v1 -> Maybe v2) -> InsOrdSymbolMap v1 -> InsOrdSymbolMap v2
mapMaybeWithKey f (InsOrdSymbolMap i m) =
    InsOrdSymbolMap i $ Map.mapMaybeWithKey f' m
  where
    f' k (P j x) = P j <$> f k x

-------------------------------------------------------------------------------
-- Conversions
-------------------------------------------------------------------------------

keys :: InsOrdSymbolMap v -> [Symbol]
keys = fmap fst . toList
{-# INLINABLE keys #-}

elems :: InsOrdSymbolMap v -> [v]
elems = fmap snd . toList
{-# INLINABLE elems #-}

fromList :: forall v. [(Symbol, v)] -> InsOrdSymbolMap v
fromList
    = mk
    . flip runState 0
    . (traverse . _2) newP
  where
    mk :: ([(Symbol, P v)], Int) -> InsOrdSymbolMap v
    mk (m, i) = InsOrdSymbolMap i (Map.fromList m)

toList :: InsOrdSymbolMap v -> [(Symbol, v)]
toList
    = fmap (second getPV)
    . sortOn (getPK . snd)
    . Map.toList
    . getInsOrdSymbolMap

toRevList :: InsOrdSymbolMap v -> [(Symbol, v)]
toRevList
    = fmap (second getPV)
    . sortOn (Down . getPK . snd)
    . Map.toList
    . getInsOrdSymbolMap

fromMap :: Map Symbol v -> InsOrdSymbolMap v
fromMap = mk . flip runState 0 . traverse newP
  where
    mk (m, i) = InsOrdSymbolMap i m

toMap :: InsOrdSymbolMap v -> Map Symbol v
toMap (InsOrdSymbolMap _ m) = fmap getPV m

-------------------------------------------------------------------------------
-- Internal
-------------------------------------------------------------------------------

-- TODO: more efficient way is to do two traversals
-- - collect the indexes
-- - pack the indexes (Map old new)
-- - traverse second time, changing the indexes
fromMapP :: Map Symbol (P v) -> InsOrdSymbolMap v
fromMapP = mk . flip runState 0 . retractSortedAp . traverse f
  where
    mk (m, i) = InsOrdSymbolMap i m
    f (P i v) = liftSortedAp i (newP v)

-- | Test if the internal map structure is valid.
valid :: InsOrdSymbolMap v -> Bool
valid (InsOrdSymbolMap i m) = indexesDistinct && indexesSmaller
  where
    indexes :: [Int]
    indexes = getPK <$> Map.elems m

    indexesDistinct = indexes == nub indexes
    indexesSmaller  = all (< i) indexes

newP :: a -> State Int (P a)
newP x = state $ \s -> (P s x, s + 1)
