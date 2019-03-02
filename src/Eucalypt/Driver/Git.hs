{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-|
Module      : Eucalypt.Driver.Git
Description : Utilities for working with git
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Driver.Git where

import Debug.Trace

import Control.Exception.Safe
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Eucalypt.Driver.Error
import Eucalypt.Syntax.Input (Input(..), Locator(..))
import Network.URI (URI, uriScheme, uriPath, parseURI)
import Path
import System.Directory (withCurrentDirectory)
import System.Process.Typed (proc, readProcess_)



-- | Determine an appropriate path for the cached repo
formCacheDir ::
     (MonadThrow m)
  => Path Abs Dir     -- ^ eucalypt directory
  -> URI              -- ^ git repo
  -> String           -- ^ git commit
  -> m (Path Abs Dir) -- ^ cache directory for the git repo
formCacheDir eucalyptd _repo commit = do
  c <- parseRelDir ".cache"
  d <- parseRelDir commit
  return $ eucalyptd </> c </> d



-- | Return an Input representing the specified file in our cache
inputForCachedFile ::
     (MonadThrow m)
  => Path Abs Dir -- ^ eucalypt directory
  -> URI          -- ^ git repo
  -> String       -- ^ git commit
  -> Input        -- ^ input before resolving against cache dir
  -> m Input      -- ^ input resolved against cache dir
inputForCachedFile eucalyptd repo commit input =
  case inputLocator input of
    URLInput u ->
      case uriScheme u of
        "file:" -> do
          cachedir <- traceShowId <$> formCacheDir eucalyptd repo commit
          rel <- parseRelFile (uriPath u)
          let filepath = toFilePath (cachedir </> rel)
          let uri = fromMaybe u (parseURI $ "file:" ++ filepath)
          return input {inputLocator = URLInput uri}
        sch ->
          throwM $
          InvalidImport $
          "URL scheme " ++ sch ++ " incompatible with git import."
    _ ->
      throwM $
      InvalidImport $ "Input " ++ show input ++ " incompatible with git import."



-- | Ensure that the specified git repo is in our cache
ensureCloned :: Path Abs Dir -- ^ eucalypt directory
  -> URI                     -- ^ git repo
  -> String                  -- ^ git commit
  -> IO (Path Abs Dir)       -- ^ directory of cached repo
ensureCloned eucalyptd repo commit = do
  cachedir <- formCacheDir eucalyptd repo commit
  void $ readProcess_ $ proc "git" ["clone", show repo, toFilePath cachedir]
  void $
    withCurrentDirectory (toFilePath cachedir) $
    readProcess_ $ proc "git" ["reset", "--hard", commit]
  return cachedir
