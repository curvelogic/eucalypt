{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.Eq
Description : Global EQ implementation in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Eq
  ( globals
  ) where

import Data.Symbol
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [(Symbol, LambdaForm)]
globals = [("EQ", euEq)]

-- | __EQ(l, r) - deep equality test in STG
euEq :: LambdaForm
euEq =
  lam_ 0 2 $
  ann_ "__EQ" 0 $
  casedef_
    (Atom (L 0))
    [ (stgNil, (0, casedef_ (Atom (L 1)) [(stgNil, (0, trueVal))] falseVal))
    , (stgUnit, (0, casedef_ (Atom (L 1)) [(stgUnit, (0, trueVal))] falseVal))
    , (stgTrue, (0, casedef_ (Atom (L 1)) [(stgTrue, (0, trueVal))] falseVal))
    , (stgFalse, (0, casedef_ (Atom (L 1)) [(stgFalse, (0, trueVal))] falseVal))
    , ( stgBlock
      , ( 1
        , casedef_
            (Atom (L 1))
            [ (stgBlock, (1, appfn_ (gref "EQ") [L 2, L 3]))
            , ( stgIOSMBlock
              , ( 1
                , force_ (appfn_ (gref "IOSM.LIST") [L 3]) $
                  appfn_ (gref "EQ") [L 2, L 4]))
            ]
            falseVal))
    , ( stgCons
      , ( 1
        , casedef_
            (Atom (L 1))
            [ ( stgCons
              , ( 1
                , let_
                    [ pc_ [L 2, L 4] $ valuen_ 2 $ appfn_ (gref "EQ") [L 0, L 1]
                    , pc_ [L 3, L 5] $ valuen_ 2 $ appfn_ (gref "EQ") [L 0, L 1]
                    ]
                    (appfn_ (gref "AND") [L 6, L 7])))
            ]
            falseVal))
    , ( stgIOSMBlock
      , ( 1
        , casedef_
            (Atom (L 1))
            [ ( stgBlock
              , ( 1
                , force_ (appfn_ (gref "IOSM.LIST") [L 2]) $
                  appfn_ (gref "EQ") [L 4, L 3]))
            , (stgIOSMBlock, (1, appfn_ (gref "IOSM.EQ") [L 2, L 3]))
            ]
            falseVal))
    ]
    (casedef_
       (Atom (L 1))
       [ (stgNil, (0, falseVal))
       , (stgUnit, (0, falseVal))
       , (stgTrue, (0, falseVal))
       , (stgFalse, (0, falseVal))
       , (stgCons, (0, falseVal))
       , (stgBlock, (0, falseVal))
       ]
       (appbif_ (intrinsicIndex "===") [L 2, L 3]))
  where
    trueVal = appcon_ stgTrue []
    falseVal = appcon_ stgFalse []
