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
  lam_ 0 2 $ ann_ "__EQ" 0 $
  casedef_
    (Atom (Local 0))
    [ ( stgNil
      , ( 0
        , casedef_
            (Atom (Local 1))
            [(stgNil, (0, trueVal))]
            falseVal))
    , ( stgUnit
      , ( 0
        , casedef_
            (Atom (Local 1))
            [(stgUnit, (0, trueVal))]
            falseVal))
    , ( stgTrue
      , ( 0
        , casedef_
            (Atom (Local 1))
            [(stgTrue, (0, trueVal))]
            falseVal))
    , ( stgFalse
      , ( 0
        , casedef_
            (Atom (Local 1))
            [(stgFalse, (0, trueVal))]
            falseVal))
    , ( stgBlock
      , ( 1
        , casedef_
            (Atom (Local 1))
            [(stgBlock, (1, appfn_ (Global "EQ") [Local 2, Local 3]))]
            falseVal))
    , ( stgCons
      , ( 1
        , casedef_
            (Atom (Local 1))
            [ ( stgCons
              , ( 1
                , let_
                    [ pc_ [Local 2, Local 4] $
                      valuen_ 2 $ appfn_ (Global "EQ") [Local 0, Local 1]
                    , pc_ [Local 3, Local 5] $
                      valuen_ 2 $ appfn_ (Global "EQ") [Local 0, Local 1]
                    ]
                    (appfn_ (Global "AND") [Local 6, Local 7])))
            ]
            falseVal))]
    (force_
       (Atom (Local 1))
       (appbif_ (intrinsicIndex "===") [Local 2, Local 3]))
  where
    trueVal = appcon_ stgTrue []
    falseVal = appcon_ stgFalse []
