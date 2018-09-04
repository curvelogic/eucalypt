{-|
Module      : Eucalypt.Stg.Globals.List
Description : Standard list globals for the STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Globals.List
  ( euCons
  , euNil
  , euHead
  , euTail
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags


-- | __CONS(h, t)
euCons :: LambdaForm
euCons = lam_ 0 2 $ appcon_ stgCons [BoundArg 0, BoundArg 1]



-- | __NIL
euNil :: LambdaForm
euNil =
  lam_ 0 1 $
  ann_ "NIL" $
  case_
    (Atom (BoundArg 0))
    [ (stgCons, (2, Atom (Literal (NativeBool False))))
    , (stgNil, (0, Atom (Literal (NativeBool True))))
    ]



-- | __HEAD(list)
euHead :: LambdaForm
euHead =
  lam_ 0 1 $
  case_ (Atom (BoundArg 0)) [(stgCons, (2, Atom (Local 1)))]



-- | __TAIL(list)
euTail :: LambdaForm
euTail =
  lam_ 0 1 $
  case_ (Atom (BoundArg 0)) [(stgCons, (2, Atom (Local 2)))]
