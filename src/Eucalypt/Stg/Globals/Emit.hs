{-|
Module      : Eucalypt.Stg.Globals.Emit
Description : Emit / render STG code
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Emit
  ( euRender
  , euNull
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

euNull :: LambdaForm
euNull = standardConstructor 0 stgUnit

euRender :: LambdaForm
euRender =
  lam_ 0 1 $ ann_ "__RENDER" $
  letrec_
      -- emptyList
    [ pc0_ $ value_ $ ann_ "emptyList" $ seq_ emitSS emitSE
        -- continueList
    , pc_ [continueList] $
      lam_ 1 1 $ ann_ "continueList" $
      casedef_
        (Atom (BoundArg 0))
        [ ( stgCons
          , ( 2
            , seq_ (appfn_ (Global "RENDER") [Local 2]) $
              appfn_ (Local 0) [Local 3]))
        , (stgNil, (0, emitSE))
        ]
        (emitScalar (Local 2))
        -- startList
    , pc_ [continueList] $
      lam_ 1 2 $ ann_ "startList" $
      seq_
        emitSS
        (seq_
           (appfn_ (Global "RENDER") [BoundArg 0])
           (appfn_ (Local 0) [BoundArg 1]))
        -- wrapBlock
    , pc_ [continueKVList] $
      lam_ 1 1 $ ann_ "wrapBlock" $ seqall_ [emitMS, appfn_ (Local 0) [BoundArg 0], emitME]
        -- renderKV (unless lambda)
    , pc_ [] $
      lam_ 0 1 $ ann_ "renderKV" $
      casedef_
        (Atom (BoundArg 0))
        [ ( stgCons
          , ( 2
            , case_
                (Atom (Local 2))
                [ ( stgCons
                  , ( 2
                    , force_
                        (Atom (Local 3))
                        (caselit_
                           (appbif_ (intrinsicIndex "CLOSED") [Local 5])
                           [ ( NativeBool True
                             , seq_
                                 (appfn_ (Global "RENDER") [Local 1])
                                 (appfn_ (Global "RENDER") [Local 5]))
                           ]
                           (Just (appcon_ stgUnit [])))))
                ]))
        ]
        (appfn_ (Global "BOMB") [])
        -- continueKVList
    , pc_ [renderKV, continueKVList] $
      lam_ 2 1 $ ann_ "continueKVList" $
      case_
        (Atom (BoundArg 0))
        [ ( stgCons
          , (2, seq_ (appfn_ (Local 0) [Local 3]) $ appfn_ (Local 1) [Local 4]))
        , (stgNil, (0, appcon_ stgUnit []))
        ]
        -- typeSwitch
    , pc_ [emptyList, continueList, startList, wrapBlock] $
      lam_ 4 1 $ ann_ "typeSwitch" $
      casedef_
        (Atom (BoundArg 0))
        [ (stgBlock, (1, appfn_ (Local 3) [Local 5]))
        , (stgCons, (2, appfn_ (Local 2) [Local 5, Local 6]))
        , (stgNil, (0, appfn_ (Local 0) []))
        , (stgUnit, (0, emitNull))
        ]
        (emitScalar (Local 5))
    ]
    (appfn_ typeSwitch [BoundArg 0])
  where
    emptyList = Local 1
    continueList = Local 2
    startList = Local 3
    wrapBlock = Local 4
    renderKV = Local 5
    continueKVList = Local 6
    typeSwitch = Local 7
    emitMS = appbif_ (intrinsicIndex "EMIT{") []
    emitME = appbif_ (intrinsicIndex "EMIT}") []
    emitSS = appbif_ (intrinsicIndex "EMIT[") []
    emitSE = appbif_ (intrinsicIndex "EMIT]") []
    emitScalar n = appbif_ (intrinsicIndex "EMITx") [n]
    emitNull = appbif_ (intrinsicIndex "EMIT0") []
