{-|
Module      : Eucalypt.Stg.Globals.Common
Description : Common utilities for global function implementations
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Common where

import Eucalypt.Stg.Intrinsics (intrinsicArity, intrinsicIndex)
import Eucalypt.Stg.Syn

-- | Create a global as a thin wrapper around an intrinsic
wrapBif :: String -> LambdaForm
wrapBif bif =
  LambdaForm 0 arity (arity == 0) $
  ann_ bif 0 $
  appbif_ (intrinsicIndex bif) [L $ fromIntegral n | n <- [0 .. (arity - 1)]]
  where
    arity = intrinsicArity bif

-- | Create a global as a thin wrapper around an intrinsic that
-- evaluates all arguments first.
wrapBifStrict :: String -> LambdaForm
wrapBifStrict bif =
  LambdaForm 0 arity (arity == 0) $
  ann_ bif 0 $ foldr forceArg call [fromIntegral n | n <- [0 .. (arity - 1)]]
  where
    arity = intrinsicArity bif
    call =
      appbif_
        (intrinsicIndex bif)
        [L $ fromIntegral n | n <- [arity .. ((2 * arity) - 1)]]

forceArg :: Word -> StgSyn -> StgSyn
forceArg n = force_ (Atom $ L n)
