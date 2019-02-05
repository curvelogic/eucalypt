{-|
Module      : Eucalypt.Stg.Intrinsics.Meta
Description : Metadata builtins for STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Intrinsics.Meta
  ( intrinsics
  ) where

import Eucalypt.Stg.Address (allocate, peek)
import Eucalypt.Stg.Error
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.IntrinsicInfo
import Eucalypt.Stg.Intrinsics.Common
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Syn

intrinsics :: [IntrinsicInfo]
intrinsics =
  [ IntrinsicInfo "META" 1 (invoke meta)
  , IntrinsicInfo "WITHMETA" 2 (invoke withMeta)
  ]

-- | Extract meta from (assumed NF) value
meta :: MachineState -> StgValue -> IO MachineState
meta ms val = do
  metaval <- case val of
    (StgNat _ m) -> return m
    (StgAddr addr) -> objectMeta <$> peek addr
  case metaval of
    (Just v) -> return $ setCode ms (Eval (Atom $ L 0) (singleton v))
    Nothing -> return $ setCode ms (Eval (Atom $ gref "KEMPTYBLOCK") mempty)

-- | Override the metadata of a value
withMeta :: MachineState -> StgValue -> StgValue -> IO MachineState
withMeta ms m v =
  case v of
    (StgNat n _) -> return $ setCode ms (ReturnLit n (Just m))
    (StgAddr a) -> do
      obj <- peek a
      newAddr <-
        case obj of
          c@Closure {} -> allocate c {closureMeta = asMeta (Just m)}
          p@PartialApplication {} -> allocate p {papMeta = asMeta (Just m)}
          _ -> throwIn ms AddMetaToBlackHole
      return $ setCode ms (Eval (Atom $ L 0) (singleton (StgAddr newAddr)))
