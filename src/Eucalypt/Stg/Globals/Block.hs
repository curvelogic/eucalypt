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
  , euDeepMerge
  , euDeepMergeIfBlocks
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


-- | Both merge builtins combined lists and the prune to handle
-- duplicates. This abstracts the common template. Provide a
-- LambdaForm that prunes the single environment entry @Local 0@.
mergeTemplate :: String -> LambdaForm -> LambdaForm
mergeTemplate name prune =
  lam_ 0 2 $
  ann_ name 0 $
  let l = Local 0
      r = Local 1
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
        , pc_ [evaled] prune
        ] $
      appcon_ stgBlock [merged]


-- | __MERGE(l, r)
euMerge :: LambdaForm
euMerge =
  mergeTemplate "__MERGE" $
  thunkn_ 1 $
  force_ (Atom $ Local 0) (appbif_ (intrinsicIndex "PRUNE") [Local 1])

-- | __DEEPMERGE(l, r)
euDeepMerge :: LambdaForm
euDeepMerge =
  mergeTemplate "__DEEPMERGE" $
  thunkn_ 1 $
  force_
    (Atom $ Local 0)
    (appbif_ (intrinsicIndex "PRUNEMERGE") [Local 1, Global "DEEPMERGEIFBLOCKS"])

-- | __DEEPMERGEIFBLOCKS
euDeepMergeIfBlocks :: LambdaForm
euDeepMergeIfBlocks =
  lam_ 0 2 $
  ann_ "__DEEPMERGEIFBLOCKS" 0 $
  casedef_
    (Atom $ Local 0)
    [ ( stgBlock
      , ( 1
        , casedef_
            (Atom $ Local 1)
            [(stgBlock, (1, appfn_ (Global "DEEPMERGE") [Local 0, Local 1]))]
            (Atom $ Local 1)))
    ]
    (Atom $ Local 1)

-- | __BLOCK(l)
euBlock :: LambdaForm
euBlock = lam_ 0 1 $ appcon_ stgBlock [Local 0]

-- | __ELEMENTS(b) - return elements in order appropriate for
-- iteration (and pruned so no duplicates...)
euElements :: LambdaForm
euElements =
  lam_ 0 1 $ ann_ "__ELEMENTS" 0 $
  casedef_
    (Atom (Local 0))
    [(stgBlock, (1, Atom (Local 1)))]
    (appfn_ (Global "PANIC") [Literal $ NativeString "ELEMENTS expects block"])



-- | __LOOKUP(symbol, block)
euLookup :: LambdaForm
euLookup =
  lam_ 0 2 $
  ann_ "__LOOKUP" 0 $
  case_
    (Atom (Local 1))
    [ ( stgBlock
      , ( 1
        , let l = Local 2
              reversed = Local 3
           in let_ [pc_ [l] $ thunkn_ 1 $ appfn_ (Global "REVERSE") [Local 0]] $
              appfn_ (Global "LOOKUPLIST") [reversed, Local 0]))
    ]


-- | __LOOKUP(symbol, default, block)
euLookupOr :: LambdaForm
euLookupOr =
  let sym = Local 0
      deft = Local 1
      blk = Local 2
   in lam_ 0 3 $
      ann_ "__LOOKUPOR" 0 $
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
  ann_ "__LOOKUPLIST" 0 $
  -- break head off list of elements
  casedef_
    (Atom (Local 0))
    [ ( stgCons
      , ( 2
             -- break head (key) of kv pair
        , casedef_
            (Atom (Local 2))
            [ ( stgCons
              , ( 2
                  -- compare k with
                , caselit_
                    (appfn_ (Global "EQ") [Local 4, Local 1])
                    [ (NativeBool True, appfn_ (Global "HEAD") [Local 5])
                    , ( NativeBool False
                      , appfn_ (Global "LOOKUPLIST") [Local 3, Local 1])
                    ]
                    Nothing))
            ]
            (appfn_ (Global "PANIC") [Literal $ NativeString "Key lookup error (non-pair)."])))
    ]
    (appfn_ (Global "!KEYNOTFOUND") [Local 1])


-- | __LOOKUPLISTOR(els, default, key)
euLookupListOr :: LambdaForm
euLookupListOr =
  let l = Local 0
      def = Local 1
      sym = Local 2
   in lam_ 0 3 $
      ann_ "__LOOKUPLISTOR" 0 $
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
