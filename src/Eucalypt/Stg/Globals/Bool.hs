{-|
Module      : Eucalypt.Stg.Globals.Bool
Description : Bool fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Bool
  ( euTrue
  , euFalse
  , euNot
  , euAnd
  , euOr
  , euIf
  ) where

import Eucalypt.Stg.Syn

-- | __TRUE
euTrue :: LambdaForm
euTrue = value_ (Atom $ Literal $ NativeBool True)



-- | __ FALSE
euFalse :: LambdaForm
euFalse = value_ (Atom $ Literal $ NativeBool False)



-- | __NOT(b)
euNot :: LambdaForm
euNot =
  lam_ 0 1 $ ann_ "__NOT" 0 $
  caselit_
    (Atom (Local 0))
    [ (NativeBool True, Atom $ Literal $ NativeBool False)
    , (NativeBool False, Atom $ Literal $ NativeBool True)
    ] Nothing



-- | __AND(l, r) - shortcircuit AND
euAnd :: LambdaForm
euAnd =
  lam_ 0 2 $ ann_ "__AND" 0 $
  caselit_
    (Atom (Local 0))
    [ (NativeBool False, Atom $ Literal $ NativeBool False)
    , ( NativeBool True
      , caselit_
          (Atom (Local 1))
          [ (NativeBool False, Atom $ Literal $ NativeBool False)
          , (NativeBool True, Atom $ Literal $ NativeBool True)
          ]
          Nothing)
    ]
    Nothing



-- | __OR(l, r) - shortcircuit OR
euOr :: LambdaForm
euOr =
  lam_ 0 2 $ ann_ "__OR" 0 $
  caselit_
    (Atom (Local 0))
    [ (NativeBool True, Atom $ Literal $ NativeBool True)
    , ( NativeBool False
      , caselit_
          (Atom (Local 1))
          [ (NativeBool True, Atom $ Literal $ NativeBool True)
          , (NativeBool False, Atom $ Literal $ NativeBool False)
          ]
          Nothing)
    ]
    Nothing



-- | __IF(c, t, f)
euIf :: LambdaForm
euIf =
  lam_ 0 3 $ ann_ "__IF" 0 $
  caselit_
    (Atom (Local 0))
    [ (NativeBool True, Atom $ Local 1)
    , (NativeBool False, Atom $ Local 2)
    ]
    Nothing
