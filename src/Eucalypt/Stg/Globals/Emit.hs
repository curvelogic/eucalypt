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

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)


globals :: [GlobalInfo]
globals =
  [ GlobalInfo "Emit.suppresses" suppresses [NonStrict]
  , GlobalInfo "Emit.renderKV" renderKV [NonStrict]
  , GlobalInfo "Emit.continueKVList" continueKVList [NonStrict]
  , GlobalInfo "Emit.emptyList" emptyList []
  , GlobalInfo "Emit.startList" startList [NonStrict, NonStrict]
  , GlobalInfo "Emit.continueList" continueList [NonStrict]
  , GlobalInfo "Emit.wrapBlock" wrapBlock [NonStrict]
  , GlobalInfo "Emit.forceExportMetadata" forceExportMetadata [NonStrict]
  , GlobalInfo "Emit.forceExportMetadataKVList" forceExportMetadataKVList [NonStrict]
  , GlobalInfo "Emit.forceKVNatPair" forceKVNatPair [NonStrict]
  , GlobalInfo "Emit.isRenderMetadataKey" isRenderMetadataKey [Strict]
  , GlobalInfo "RENDER" euRender [NonStrict]
  , GlobalInfo "NULL" euNull []
  ]


panic :: String -> StgSyn
panic msg = appfn_ (Global "PANIC") [Literal $ NativeString msg]

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
        , caselit_
            (appfn_ (Global "Emit.suppresses") [meta])
            [(NativeBool True, appcon_ stgUnit [])]
            (Just $
             case_
               (Atom t)
               [ ( stgCons
                 , ( 2
                   , force_
                       (Atom val)
                       (caselit_
                          (appbif_ (intrinsicIndex "CLOSED") [vval])
                          [ ( NativeBool True
                            , seq_
                                (appfn_ (Global "RENDER") [key])
                                (appfn_ (Global "RENDER") [vval]))
                          ]
                          (Just (appcon_ stgUnit [])))))
               ])))
    ]
    (appfn_
       (Global "PANIC")
       [Literal $ NativeString "Bad pair in Emit.renderKV"])
  where
    arg = Local 0
    key = Local 1
    t = Local 2
    meta = Local 3
    _suppressed = Local 4
    val = Local 5
    _valt = Local 6
    vval = Local 7

-- | LambdaForm that Inspects the metadata argument supplied to
-- determine if it suppresses render.
--
-- __Emit.suppresses(m)
suppresses :: LambdaForm
suppresses =
  lam_ 0 1 $
  ann_ "Emit.suppresses" 0 $
  casedef_
    (Atom (Local 0))
    [ ( stgBlock
      , ( 1
        , caselit_
            (appfn_
               (Global "LOOKUPOR")
               [ Literal $ NativeSymbol "export"
               , Literal $ NativeSymbol "enable"
               , Local 0
               ])
            [(NativeSymbol "suppress", Atom $ Literal $ NativeBool True)]
            (Just $ Atom $ Literal $ NativeBool False)))
    ] $
  Atom $ Literal $ NativeBool False

emptyList :: LambdaForm
emptyList = value_ $ ann_ "emptyList" 0 $ seq_ emitSS emitSE

-- | Emit.continueList(l)
continueList :: LambdaForm
continueList =
  lam_ 0 1 $
  ann_ "Emit.continueList" 0 $
  casedef_
    (Atom (Local 0))
    [ ( stgCons
      , ( 2
        , seq_ (appfn_ (Global "RENDER") [Local 1]) $
          appfn_ (Global "Emit.continueList") [Local 2]))
    , (stgNil, (0, emitSE))
    ] $
  force_ (appfn_ (Global "META") [Local 1]) $
  force_ (appfn_ (Global "Emit.forceExportMetadata") [Local 2]) $
  emitScalar (Local 1) -- force is effectful

-- | Emit.startList(l)
startList :: LambdaForm
startList =
  lam_ 0 2 $
  ann_ "Emit.startList" 0 $
  seq_
    emitSS
    (seq_
       (appfn_ (Global "RENDER") [Local 0])
       (appfn_ (Global "Emit.continueList") [Local 1]))

-- | __Emit.continueKVList(l)
continueKVList :: LambdaForm
continueKVList =
  lam_ 0 1 $
  ann_ "Emit.continueKVList" 0 $
  case_
    (Atom (Local 0))
    [ ( stgCons
      , ( 2
        , seq_ (appfn_ (Global "Emit.renderKV") [Local 1]) $
          appfn_ (Global "Emit.continueKVList") [Local 2]))
    , (stgNil, (0, appcon_ stgUnit []))
    ]

-- | __Emit.wrapBlock(b)
wrapBlock :: LambdaForm
wrapBlock =
  lam_ 0 1 $
  ann_ "Emit.wrapBlock" 0 $
  seqall_ [emitMS, appfn_ (Global "Emit.continueKVList") [Local 0], emitME]


-- | __RENDER(v)
euRender :: LambdaForm
euRender =
  lam_ 0 1 $
  ann_ "__RENDER" 0 $
  casedef_
    (Atom (Local 0))
    [ (stgBlock, (1, appfn_ (Global "Emit.wrapBlock") [Local 1]))
    , (stgCons, (2, appfn_ (Global "Emit.startList") [Local 1, Local 2]))
    , (stgNil, (0, appfn_ (Global "Emit.emptyList") []))
    , (stgUnit, (0, emitNull))
    ] $
  force_ (appfn_ (Global "META") [Local 1]) $
  force_ (appfn_ (Global "Emit.forceExportMetadata") [Local 2]) $
  emitScalar (Local 1)

-- | Single argument is the metadata (not the annotated value)
forceExportMetadata :: LambdaForm
forceExportMetadata =
  lam_ 0 1 $
  ann_ "Emit.forceExportMetadata" 0 $
  let b = Local 0
      l = Local 1
   in case_
        (Atom b)
        [ ( stgBlock
          , ( 1
            , force_
                (appfn_ (Global "Emit.forceExportMetadataKVList") [l])
                (appcon_ stgUnit [])))
        ]


forceExportMetadataKVList :: LambdaForm
forceExportMetadataKVList =
  lam_ 0 1 $
  ann_ "Emit.forceExportMetadataKVList" 0 $
  let l = Local 0
      h = Local 1
      t = Local 2
   in case_
        (Atom l)
        [ (stgNil, (0, Atom (Global "KNIL")))
        , ( stgCons
          , ( 2
            , force_ (appfn_ (Global "Emit.forceKVNatPair") [h]) $
              force_ (appfn_ (Global "Emit.forceExportMetadataKVList") [t]) $
              appcon_ stgUnit []))
        ]


isRenderMetadataKey :: LambdaForm
isRenderMetadataKey =
  lam_ 0 1 $
  ann_ "Emit.isRenderMetadataKey" 0 $
  caselit_
    (Atom (Local 0))
    [ (NativeSymbol "export", Atom (Global "TRUE"))
    , (NativeSymbol "tag", Atom (Global "TRUE"))
    ] $
  Just (Atom (Global "FALSE"))


forceKVNatPair :: LambdaForm
forceKVNatPair =
  lam_ 0 1 $
  ann_ "Emit.forceKVNatPair" 0 $
  let pr = Local 0
      prh = Local 1
   in casedef_
        (Atom pr)
        [ ( stgCons
          , ( 2
            , caselit_
                (appfn_ (Global "Emit.isRenderMetadataKey") [prh])
                [(NativeBool True, appfn_ (Global "seqNatList") [pr])]
                (Just (Atom pr))))
        ]
        (panic "Invalid pair (not cons) while evaluating render metadata")
