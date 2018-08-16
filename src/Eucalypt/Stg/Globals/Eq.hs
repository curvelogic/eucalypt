{-|
Module      : Eucalypt.Stg.Globals.Eq
Description : Global EQ implementation in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Eq
  ( euEq
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

-- | __EQ(l, r) - deep equality test in STG
euEq :: LambdaForm
euEq =
  lam_ 0 2 $
  casedef_
    (Atom (BoundArg 0))
    [ ( stgNil
      , ( 0
        , casedef_
            (Atom (BoundArg 1))
            [(stgNil, (0, Atom (Literal $ NativeBool True)))]
            (Atom $ Literal $ NativeBool False)))
    , ( stgUnit
      , ( 0
        , casedef_
            (Atom (BoundArg 1))
            [(stgUnit, (0, Atom (Literal $ NativeBool True)))]
            (Atom $ Literal $ NativeBool False)))
    , ( stgBlock
      , ( 1
        , casedef_
            (Atom (BoundArg 1))
            [(stgBlock, (1, appfn_ (Global "EQ") [Local 2, Local 3]))]
            (Atom $ Literal $ NativeBool False)))
    , ( stgCons
      , ( 1
        , casedef_
            (Atom (BoundArg 1))
            [ ( stgCons
              , ( 1
                , let_
                    [ pc_ [Local 2, Local 4] $
                      thunkn_ 2 $ appfn_ (Global "EQ") [Local 0, Local 1]
                    , pc_ [Local 3, Local 5] $
                      thunkn_ 2 $ appfn_ (Global "EQ") [Local 0, Local 1]
                    ]
                    (appfn_ (Global "AND") [Local 6, Local 7])))
            ]
            (Atom $ Literal $ NativeBool False)))
    ]
    (force_
       (Atom (BoundArg 1))
       (appbif_ (intrinsicIndex "===") [Local 2, Local 3]))
