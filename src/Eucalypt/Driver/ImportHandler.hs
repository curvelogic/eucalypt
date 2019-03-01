{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Driver.ImportHandler
Description : Types of import that the driver can read, resolve and load
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Driver.ImportHandler
  ( importHandler
  ) where

import Data.Maybe (fromMaybe, maybeToList)
import Eucalypt.Core.Import
import Eucalypt.Core.Metadata
  ( pruneUnevaluatedMetadata
  , readUnevaluatedMetadata
  )
import Eucalypt.Core.Syn
import Eucalypt.Driver.Git (inputForCachedFile)
import Eucalypt.Syntax.Input
  ( Input(..)
  , parseInputFromString
  )
import Network.URI
import Path



-- | Imports can be specified in a variety of ways. Simple imports are
-- resolved relative to the load path. Git imports need caching and
-- are converted into 'Input's once cached.
data ImportSpecification
  = SimpleImport { input :: Input }
  | GitImport { repo :: URI
              , commit :: String
              , input :: Input }


-- | Read 'ImportSpecification's from core metadata
importsFromMetadata :: CoreExp a -> Maybe [ImportSpecification]
importsFromMetadata m = readUnevaluatedMetadata "import" m extract
  where
    extract (CorePrim _ (CoreString s)) =
      map SimpleImport $ maybeToList $ parseInputFromString s
    extract (CoreList _ l) = concatMap extract l
    extract _ = []



-- | Resolve the imports down to inputs which identify the locations
-- once remote libs have been cached
resolveToInput :: Path Abs Dir -> ImportSpecification -> Input
resolveToInput _ SimpleImport {input = i} = i
resolveToInput eucalyptd GitImport {..} =
  fromMaybe input $ inputForCachedFile eucalyptd repo commit input



-- | An import handler to wrap up these capabilities
importHandler :: Path Abs Dir -> ImportHandler
importHandler eucalyptd = ImportHandler
      { readImports =
          fmap (fmap (resolveToInput eucalyptd)) . importsFromMetadata
      , pruneImports = pruneUnevaluatedMetadata "import"
      }
