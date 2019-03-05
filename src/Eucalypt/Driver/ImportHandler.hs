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


import Control.Arrow ((&&&))
import Control.Monad (void)
import Data.Maybe (fromMaybe, maybeToList)
import Eucalypt.Core.Import
import Eucalypt.Core.Metadata
  ( pruneUnevaluatedMetadata
  , readUnevaluatedMetadata
  )
import Eucalypt.Core.Syn
import Eucalypt.Driver.Git (ensureCloned, inputForCachedFile)
import Eucalypt.Syntax.Input
  ( Input(..)
  , parseInputFromString
  )
import Path



-- | Imports can be specified in a variety of ways. Simple imports are
-- resolved relative to the load path. Git imports need caching and
-- are converted into 'Input's once cached.
data ImportSpecification
  = SimpleImport { input :: Input }
  | GitImport { repo :: String
              , commit :: String
              , input :: Input }
  deriving (Eq, Show)


-- | Read 'ImportSpecification's from core metadata
importsFromMetadata :: CoreExp a -> Maybe [ImportSpecification]
importsFromMetadata m = readUnevaluatedMetadata "import" m extract
  where
    extract (CorePrim _ (CoreString s)) =
      map SimpleImport $ maybeToList $ parseInputFromString s
    extract (CoreList _ l) = concatMap extract l
    extract b@CoreBlock {} = fromMaybe [] $ extractGitImports b
    extract b@CoreLet {} = fromMaybe [] $ extractGitImports b
    extract _ = []



-- | Extract git import from a metadata block inside the import list
extractGitImports :: CoreExp a -> Maybe [ImportSpecification]
extractGitImports m = do
  repo <- readUnevaluatedMetadata "git" m fromStr
  commit <- readUnevaluatedMetadata "commit" m fromStr
  inputs <- readUnevaluatedMetadata "import" m extract
  return $ map (GitImport repo commit) inputs
  where
    fromStr (CorePrim _ (CoreString s)) = s
    fromStr _ = []
    extract (CorePrim _ (CoreString s)) = maybeToList $ parseInputFromString s
    extract (CoreList _ l) = concatMap extract l
    extract _ = []



-- | Resolve the imports down to inputs which identify the locations
-- once remote libs have been cached
toInput :: Path Abs Dir -> ImportSpecification -> Input
toInput _ SimpleImport {input = i} = i
toInput eucalyptd GitImport {..} =
  fromMaybe input $ inputForCachedFile eucalyptd repo commit input



-- | The IO action to cache the repo so the inputs are valid
toAction :: Path Abs Dir -> ImportSpecification -> IO ()
toAction _ SimpleImport {} = return ()
toAction eucalyptd GitImport {..} = void $ ensureCloned eucalyptd repo commit



readImportSpecs :: Path Abs Dir -> CoreExp a -> [(Input, IO ())]
readImportSpecs eucalyptd core =
  let imports = fromMaybe [] $ importsFromMetadata core
   in map (toInput eucalyptd &&& toAction eucalyptd) imports




-- | An import handler to wrap up these capabilities
importHandler :: Path Abs Dir -> ImportHandler
importHandler eucalyptd =
  ImportHandler
    { readImports = readImportSpecs eucalyptd
    , pruneImports = pruneUnevaluatedMetadata "import"
    }
