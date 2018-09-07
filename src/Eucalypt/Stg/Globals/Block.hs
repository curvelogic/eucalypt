{-|
Module      : Eucalypt.Stg.Globals.Block
Description : Block fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Block
  ( euMerge
  , euBlock
  , euElements
  , euLookup
  , euLookupOr
  , euLookupList
  , euLookupListOr
  ) where

import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- | __MERGE(l, r)
euMerge :: LambdaForm
euMerge =
  lam_ 0 2 $
  ann_ "__MERGE" $
  let l = BoundArg 0
      r = BoundArg 1
      lel = Local 2
      rel = Local 3
      els = Local 4
      evaled = Local 5
      merged = Local 6
   in letrec_
        [ pc_ [l] $ thunkn_ 1 $ appfn_ (Global "ELEMENTS") [Local 0]
        , pc_ [r] $ thunkn_ 1 $ appfn_ (Global "ELEMENTS") [Local 0]
        , pc_ [lel, rel] $
          thunkn_ 2 $ appfn_ (Global "CONCAT") [Local 0, Local 1]
        , pc_ [els] $ thunkn_ 1 $ appfn_ (Global "seqPairList") [Local 0]
        , pc_ [evaled] $
          thunkn_ 1 $
          force_ (Atom $ Local 0) (appbif_ (intrinsicIndex "PRUNE") [Local 1])
        ] $
      appcon_ stgBlock [merged]

-- | __BLOCK(l)
euBlock :: LambdaForm
euBlock = lam_ 0 1 $ appcon_ stgBlock [BoundArg 0]

-- | __ELEMENTS(b) - return elements in order appropriate for
-- iteration (and pruned so no duplicates...)
euElements :: LambdaForm
euElements =
  lam_ 0 1 $ ann_ "__ELEMENTS" $
  casedef_
    (Atom (BoundArg 0))
    [(stgBlock, (1, Atom (Local 1)))]
    (appfn_ (Global "PANIC") [Literal $ NativeString "ELEMENTS expects block"])



-- | __LOOKUP(symbol, block)
euLookup :: LambdaForm
euLookup =
  lam_ 0 2 $
  ann_ "__LOOKUP" $
  case_
    (Atom (BoundArg 1))
    [ ( stgBlock
      , ( 1
        , let l = Local 2
              reversed = Local 3
           in let_ [pc_ [l] $ thunkn_ 1 $ appfn_ (Global "REVERSE") [Local 0]] $
              appfn_ (Global "LOOKUPLIST") [reversed, BoundArg 0]))
    ]


-- | __LOOKUP(symbol, default, block)
euLookupOr :: LambdaForm
euLookupOr =
  let sym = BoundArg 0
      deft = BoundArg 1
      blk = BoundArg 2
   in lam_ 0 3 $
      ann_ "__LOOKUPOR" $
      case_
        (Atom blk)
        [ ( stgBlock
          , ( 1
            , let els = Local 2
                  reversed = Local 3
               in let_
                    [ pc_ [els] $
                      thunkn_ 1 $ appfn_ (Global "REVERSE") [Local 0]
                    ] $
                  appfn_ (Global "LOOKUPLISTOR") [reversed, deft, sym]))
        ]



-- | __LOOKUPLIST(els, key)
euLookupList :: LambdaForm
euLookupList =
  lam_ 0 2 $
  ann_ "__LOOKUPLIST" $
  -- break head off list of elements
  case_
    (Atom (BoundArg 0))
    [ ( stgCons
      , ( 2
             -- break head (key) of kv pair
        , case_
            (Atom (Local 2))
            [ ( stgCons
              , ( 2
                  -- compare k with
                , caselit_
                    (appfn_ (Global "EQ") [Local 4, BoundArg 1])
                    [ (NativeBool True, appfn_ (Global "HEAD") [Local 5])
                    , ( NativeBool False
                      , appfn_ (Global "LOOKUPLIST") [Local 3, BoundArg 1])
                    ]
                    Nothing))
            ]))
    ]


-- | __LOOKUPLISTOR(els, default, key)
euLookupListOr :: LambdaForm
euLookupListOr =
  let l = BoundArg 0
      def = BoundArg 1
      sym = BoundArg 2
   in lam_ 0 3 $
      ann_ "__LOOKUPLISTOR" $
      case_
        (Atom l)
        [ ( stgCons
          , ( 2
            , let h = Local 3
                  t = Local 4
               in case_
                    (Atom h)
                    [ ( stgCons
                      , ( 2
                        , let k = Local 5
                              v = Local 6
                           in caselit_
                                (appfn_ (Global "EQ") [k, sym])
                                [ (NativeBool True, appfn_ (Global "HEAD") [v])
                                , ( NativeBool False
                                  , appfn_ (Global "LOOKUPLISTOR") [t, def, sym])
                                ]
                                Nothing))
                    ]))
        , (stgNil, (1, Atom def))
        ]
