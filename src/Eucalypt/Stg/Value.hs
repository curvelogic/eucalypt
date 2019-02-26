{-|
Module      : Eucalypt.Stg.Machine
Description : Values and heap objects for the STG machine
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}
module Eucalypt.Stg.Value where

import Eucalypt.Stg.Address
import Eucalypt.Stg.CallStack
import Eucalypt.Stg.Native
import Eucalypt.Stg.Pretty
import Eucalypt.Stg.Syn
import qualified Text.PrettyPrint as P


-- | Address type where mutable heap objects are allocated
type Address = AbstractAddress HeapObject

-- | Values on the stack or in environments can be addresses or
-- primitives. All 'Ref's are resolved to 'StgValues' within the
-- machine.
data StgValue
  = StgAddr !Address
  | StgNat !Native !(Maybe StgValue)
  deriving (Eq, Show)

nativeToValue :: Native -> StgValue
nativeToValue n = StgNat n Nothing

instance StgPretty StgValue where
  prettify (StgAddr _) = P.text "@"
  prettify (StgNat n Nothing) = prettify n
  prettify (StgNat n (Just m)) = prettify n <> P.char '`' <> prettify m <> P.char '`'


-- | Metadata as referenced from a closure
--
-- References to metadata may be attached to heap objects and natives.
-- This data does not form part of the value, is not evaluated except
-- when explicitly forced and is irrelevant for object equality.
--
-- It can be used to control formatting during render, give hints to
-- evaluation and diagnostic functionality etc.
data HeapObjectMetadata
  = MetadataBlank -- ^ blank whatever metadata is attached to the value
  | MetadataPassThrough -- ^ use whatever metadata is attached to the value
  | MetadataValue !StgValue -- ^ override value's metadata
  deriving (Eq, Show)

instance Semigroup HeapObjectMetadata where
  (<>) _ MetadataBlank = MetadataBlank
  (<>) l MetadataPassThrough = l
  (<>) _ r = r

asMeta :: Maybe StgValue -> HeapObjectMetadata
asMeta Nothing = MetadataBlank
asMeta (Just v) = MetadataValue v

asMetaOrPass :: Maybe StgValue -> HeapObjectMetadata
asMetaOrPass Nothing = MetadataPassThrough
asMetaOrPass (Just v) = MetadataValue v

fromMeta :: HeapObjectMetadata -> Maybe StgValue
fromMeta (MetadataValue v) = Just v
fromMeta _ = Nothing

-- | Anything storable in an 'Address'.
data HeapObject
  = Closure { closureCode :: !LambdaForm
            , closureEnv :: !ValVec
            , closureCallStack :: !CallStack
            , closureMeta :: !HeapObjectMetadata }
  | PartialApplication { papCode :: !LambdaForm
                       , papEnv :: !ValVec
                       , papArgs :: !ValVec
                       , papArity :: !Int
                       , papCallStack :: !CallStack
                       , papMeta :: !HeapObjectMetadata }
  | BlackHole
  deriving (Eq, Show)

objectMeta :: HeapObject -> Maybe StgValue
objectMeta obj = case obj of
  Closure { closureMeta = (MetadataValue v) } -> Just v
  PartialApplication { papMeta = (MetadataValue v) } -> Just v
  _ -> Nothing

instance StgPretty HeapObject where
  prettify (Closure lf env _ _) = prettify env <> P.space <> prettify lf
  prettify (PartialApplication lf env args arity _ _) =
    prettify env <> P.space <>
    P.parens
      (prettify args <>
       P.hcat
         (P.punctuate P.space (replicate (fromIntegral arity) (P.text "?")))) <>
    prettify lf
  prettify BlackHole = P.text "•"


-- | Sequence of values, used for both local environment and arrays of
-- resolved arguments.
type ValVec = Vec StgValue

extendEnv :: ValVec -> ValVec -> (ValVec, SynVec)
extendEnv env args = (env', rs)
  where
    envlen = envSize env
    arglen = envSize args
    env' = env <> args
    rs = range envlen (arglen + envlen)
