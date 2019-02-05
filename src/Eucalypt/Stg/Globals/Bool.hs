{-|
Module      : Eucalypt.Stg.Globals.Bool
Description : Bool fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Bool
  ( globals
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

globals :: [(String, LambdaForm)]
globals =
  [ ("TRUE", euTrue)
  , ("FALSE", euFalse)
  , ("NOT", euNot)
  , ("AND", euAnd)
  , ("OR", euOr)
  , ("IF", euIf)
  ]

falseBranch :: (Int, StgSyn)
falseBranch = (0, appcon_ stgFalse [])

trueBranch :: (Int, StgSyn)
trueBranch = (0, appcon_ stgTrue [])

-- | __TRUE
euTrue :: LambdaForm
euTrue = trueConstructor



-- | __ FALSE
euFalse :: LambdaForm
euFalse = falseConstructor



-- | __NOT(b)
euNot :: LambdaForm
euNot =
  lam_ 0 1 $
  ann_ "__NOT" 0 $
  case_
    (Atom (L 0))
    [(stgTrue, falseBranch), (stgFalse, trueBranch)]



-- | __AND(l, r) - shortcircuit AND
euAnd :: LambdaForm
euAnd =
  lam_ 0 2 $
  ann_ "__AND" 0 $
  case_
    (Atom (L 0))
    [ (stgFalse, falseBranch)
    , ( stgTrue
      , ( 0
        , case_
            (Atom (L 1))
            [(stgFalse, falseBranch), (stgTrue, trueBranch)]))
    ]



-- | __OR(l, r) - shortcircuit OR
euOr :: LambdaForm
euOr =
  lam_ 0 2 $
  ann_ "__OR" 0 $
  case_
    (Atom (L 0))
    [ (stgTrue, trueBranch)
    , ( stgFalse
      , ( 0
        , case_
            (Atom (L 1))
            [(stgTrue, trueBranch), (stgFalse, falseBranch)]))
    ]



-- | __IF(c, t, f)
euIf :: LambdaForm
euIf =
  lam_ 0 3 $
  ann_ "__IF" 0 $
  case_
    (Atom (L 0))
    [(stgTrue, (0, Atom $ L 1)), (stgFalse, (0, Atom $ L 2))]
