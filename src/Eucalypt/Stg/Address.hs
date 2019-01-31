{-|
Module      : Eucalypt.Stg.Address
Description : Addresses and abstract memory operations for the STG machine
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Address where

import Control.Monad.IO.Class
import Data.IORef


-- | A mutable refence to a heap object
newtype AbstractAddress a =
  Address (IORef a)
  deriving (Eq)

instance Show (AbstractAddress a) where
  show _ = "0x?"

-- | Allocate a new heap object, return address
allocate :: MonadIO m => a -> m (AbstractAddress a)
allocate obj = liftIO $ Address <$> newIORef obj

-- | Replace the heap object at this address
poke :: MonadIO m => AbstractAddress a -> a -> m ()
poke (Address r) = liftIO . writeIORef r

-- | Retrieve the heap object at this address
peek :: MonadIO m => AbstractAddress a -> m a
peek (Address r) = liftIO $ readIORef r
