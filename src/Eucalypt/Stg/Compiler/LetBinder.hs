{-# LANGUAGE RecordWildCards #-}
{-|
Module      : Eucalypt.Stg.Compiler.LetBinder
Description : State monad for accumulating let bindings
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Compiler.LetBinder where

import Control.Monad.State.Strict
import Data.Bifunctor (second)
import Eucalypt.Stg.Syn

-- | Often we can create PreClosures right away, occasionally we must
-- defer until we know the size of the environment created by the let
-- binding in progress.
data LetBinding = Complete PreClosure | Pending (Int -> PreClosure)

-- | State during creation of a let binder.
data LetBindState = LetBindState
  { lbNextIndex :: Word
  , lbRevBindings :: [LetBinding]
  }

initState :: Word -> LetBindState
initState es = LetBindState es []

finishState :: LetBindState -> [PreClosure]
finishState LetBindState {..} = map complete $ reverse lbRevBindings
  where
    complete (Complete pc) = pc
    complete (Pending dpc) = dpc $ fromIntegral lbNextIndex

withBinding :: PreClosure -> LetBindState -> LetBindState
withBinding pc s@LetBindState {..} =
  s {lbNextIndex = lbNextIndex + 1, lbRevBindings = Complete pc : lbRevBindings}

withDeferredBinding :: (Int -> PreClosure) -> LetBindState -> LetBindState
withDeferredBinding dpc s@LetBindState {..} =
  s {lbNextIndex = lbNextIndex + 1, lbRevBindings = Pending dpc : lbRevBindings}


-- | Monad type
type LetBinder = State LetBindState

addBinding :: PreClosure -> LetBinder Ref
addBinding pc = do
  r <- L<$> gets lbNextIndex
  modify $ withBinding pc
  return r

deferBinding :: (Int -> PreClosure) -> LetBinder Ref
deferBinding pc = do
  r <- L<$> gets lbNextIndex
  modify $ withDeferredBinding pc
  return r

-- | Executing the monad
runLetBinder :: LetBinder a -> Int -> (a, [PreClosure])
runLetBinder m envSz =
  second finishState $ runState m (initState $ fromIntegral envSz)
