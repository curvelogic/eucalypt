{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.Emit
Description : Emit / render STG code
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Emit
  ( globals
  ) where

import Data.Symbol
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)


globals :: [(Symbol, LambdaForm)]
globals =
  [ ("Emit.suppresses", suppresses)
  , ("Emit.renderKV", renderKV)
  , ("Emit.continueKVList", continueKVList)
  , ("Emit.startList", startList)
  , ("Emit.continueList", continueList)
  , ("Emit.forceExportMetadata", forceExportMetadata)
  , ("RENDER", euRender)
  , ("NULL", euNull)
  ]


panic :: String -> StgSyn
panic msg = appfn_ (gref "PANIC") [V $ NativeString msg]

-- | __NULL - for emitting JSON / YAML null
euNull :: LambdaForm
euNull = standardConstructor 0 stgUnit

emitMS :: StgSyn
emitMS = appbif_ (intrinsicIndex "EMIT{") []

emitME :: StgSyn
emitME = appbif_ (intrinsicIndex "EMIT}") []

emitSS :: StgSyn
emitSS = appbif_ (intrinsicIndex "EMIT[") []

emitSE :: StgSyn
emitSE = appbif_ (intrinsicIndex "EMIT]") []

-- | Emit a scalar.
--
-- The intrinsic requires that the value is already resolve to a
-- native with metadata and that the metadata is already evaluated
-- at all the relevant metadata keys (e.g. :export, :tag ...)
emitScalar :: Ref -> StgSyn
emitScalar n = appbif_ (intrinsicIndex "EMITx") [n]

emitNull :: StgSyn
emitNull = appbif_ (intrinsicIndex "EMIT0") []

emitTrue :: StgSyn
emitTrue = appbif_ (intrinsicIndex "EMITT") []

emitFalse :: StgSyn
emitFalse = appbif_ (intrinsicIndex "EMITF") []

-- | LambdaForm used in 'euRender' to render a key/value pair.
--
-- Require env ref for 'suppresses' function
renderKV :: LambdaForm
renderKV =
  lam_ 0 1 $
  ann_ "renderKV" 0 $
  casedef_
    (Atom arg)
    [ ( stgCons
      , ( 3
        , casedef_
            (appfn_ (gref "Emit.suppresses") [meta])
            [(stgTrue, (0, appcon_ stgUnit []))]
            (casedef_
               (Atom t)
               [ ( stgCons
                 , ( 2
                   , force_
                       (Atom val)
                       (casedef_
                          (appbif_ (intrinsicIndex "SATURATED") [vval])
                          [ ( stgTrue
                            , ( 0
                              , force_
                                  (appbif_ (intrinsicIndex "META") [vval])
                                  (casedef_
                                     (appfn_ (gref "Emit.suppresses") [valmeta])
                                     [(stgTrue, (0, appcon_ stgUnit []))]
                                     (force_
                                        (appfn_ (gref "RENDER") [key])
                                        (appfn_ (gref "RENDER") [vval])))))
                          ]
                          (appcon_ stgUnit []))))
               ]
               (panic "Bad KV in render KV"))))
    ]
    (panic "Bad pair in Emit.renderKV")
  where
    arg = L 0
    key = L 1
    t = L 2
    meta = L 3
    _suppressed = L 4
    val = L 5
    _valt = L 6
    vval = L 7
    valmeta = L 8

-- | LambdaForm that Inspects the metadata argument supplied to
-- determine if it suppresses render.
--
-- __Emit.suppresses(m)
suppresses :: LambdaForm
suppresses =
  lam_ 0 1 $
  ann_ "Emit.suppresses" 0 $
  force_ (appfn_ (gref "LOOKUPOR") [V $ NativeSymbol "export", V $ NativeSymbol "enable", L 0]) $
  appfn_ (gref "EQ") [V $ NativeSymbol "suppress", L 1]

-- | Emit.continueList(l)
continueList :: LambdaForm
continueList =
  lam_ 0 1 $
  ann_ "Emit.continueList" 0 $
  case_
    (Atom (L 0))
    [ ( stgCons
      , ( 2
        , force_ (appfn_ (gref "RENDER") [L 1]) $
          appfn_ (gref "Emit.continueList") [L 2]))
    , (stgNil, (0, emitSE))
    ]

-- | Emit.startList(l)
startList :: LambdaForm
startList =
  lam_ 0 2 $
  ann_ "Emit.startList" 0 $
  force_
    emitSS
    (force_
       (appfn_ (gref "RENDER") [L 0])
       (appfn_ (gref "Emit.continueList") [L 1]))

-- | __Emit.continueKVList(l)
continueKVList :: LambdaForm
continueKVList =
  lam_ 0 1 $
  ann_ "Emit.continueKVList" 0 $
  case_
    (Atom (L 0))
    [ ( stgCons
      , ( 2
        , force_ (appfn_ (gref "Emit.renderKV") [L 1]) $
          appfn_ (gref "Emit.continueKVList") [L 2]))
    , (stgNil, (0, appcon_ stgUnit []))
    ]

-- | __RENDER(v)
euRender :: LambdaForm
euRender =
  lam_ 0 1 $
  ann_ "__RENDER" 0 $
  casedef_
    (Atom (L 0))
    [ ( stgBlock
      , ( 1
        , force_ (appfn_ (gref "ALIST.PRUNE") [L 1]) $
          forceall_ [emitMS, appfn_ (gref "Emit.continueKVList") [L 2], emitME]))
    , (stgCons, (2, appfn_ (gref "Emit.startList") [L 1, L 2]))
    , (stgNil, (0, force_ emitSS emitSE))
    , (stgUnit, (0, emitNull))
    , (stgTrue, (0, emitTrue))
    , (stgFalse, (0, emitFalse))
    , ( stgIOSMBlock
      , ( 1
        , force_ (appfn_ (gref "IOSM.LIST") [L 1]) $
          forceall_ [emitMS, appfn_ (gref "Emit.continueKVList") [L 2], emitME]))
    ] $
  force_ (appfn_ (gref "META") [L 1]) $
  force_ (appfn_ (gref "Emit.forceExportMetadata") [L 2]) $ emitScalar (L 1)


-- | Single argument is the metadata (not the annotated value)
forceExportMetadata :: LambdaForm
forceExportMetadata =
  lam_ 0 1 $
  ann_ "Emit.forceExportMetadata" 0 $
  appfn_ (gref "LOOKUPOR") [V $ NativeSymbol "render", gref "NULL", L 0]
