{-|
Module      : Eucalypt.Stg.Intrinsics.Meta
Description : Metadata builtins for STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Meta
  ( meta
  , withMeta
  ) where

import Data.Vector ((!))
import Eucalypt.Stg.Error
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Syn

-- | Extract meta from (assumed NF) value
meta :: MachineState -> ValVec -> IO MachineState
meta ms (ValVec xs) = do
  metaval <- case xs ! 0 of
    (StgNat _ m) -> return m
    (StgAddr addr) -> objectMeta <$> peek addr
  case metaval of
    (Just v) -> return $ setCode ms (Eval (Atom $ Local 0) (singleton v))
    Nothing -> return $ setCode ms (Eval (Atom $ Global "KEMPTYBLOCK") mempty)

-- | Override the metadata of a value
withMeta :: MachineState -> ValVec -> IO MachineState
withMeta ms (ValVec xs) = do
  let m = xs ! 0
  let v = xs ! 1
  case v of
    (StgNat n _) -> return $ setCode ms (ReturnLit n (Just m))
    (StgAddr a) -> do
      obj <- peek a
      newAddr <-
        case obj of
          c@Closure {} -> allocate c {closureMeta = asMeta (Just m)}
          p@PartialApplication {} -> allocate p {papMeta = asMeta (Just m)}
          _ -> throwIn ms AddMetaToBlackHole
      return $ setCode ms (Eval (Atom $ Local 0) (singleton (StgAddr newAddr)))
