{-|
Module      : Eucalypt.Stg.Globals.Block
Description : Block fns in STG
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Block
  ( globals
  ) where

import Eucalypt.Stg.GlobalInfo (gref)
import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

globals :: [(String, LambdaForm)]
globals =
  [ ("BLOCK", euBlock)
  , ("ELEMENTS", euElements)
  , ("MERGE", euMerge)
  , ("DEEPMERGE", euDeepMerge)
  , ("DEEPMERGEIFBLOCKS", euDeepMergeIfBlocks)
  , ("LOOKUP", euLookup)
  , ("LOOKUPLIST", euLookupList)
  , ("LOOKUPOR", euLookupOr)
  , ("LOOKUPLISTOR", euLookupListOr)
  ]

-- | Both merge builtins combined lists and the prune to handle
-- duplicates. This abstracts the common template. Provide a
-- LambdaForm that prunes the single environment entry @L 0@.
mergeTemplate :: String -> LambdaForm -> LambdaForm
mergeTemplate name prune =
  lam_ 0 2 $
  ann_ name 0 $
  let l = L 0
      r = L 1
      lel = L 2
      rel = L 3
      els = L 4
      evaled = L 5
      merged = L 6
   in letrec_
        [ pc_ [l] $ thunkn_ 1 $ appfn_ (gref "ELEMENTS") [L 0]
        , pc_ [r] $ thunkn_ 1 $ appfn_ (gref "ELEMENTS") [L 0]
        , pc_ [lel, rel] $
          thunkn_ 2 $ appfn_ (gref "CONCAT") [L 0, L 1]
        , pc_ [els] $ thunkn_ 1 $ appfn_ (gref "seqPairList") [L 0]
        , pc_ [evaled] prune
        ] $
      appcon_ stgBlock [merged]


-- | __MERGE(l, r)
euMerge :: LambdaForm
euMerge =
  mergeTemplate "__MERGE" $
  thunkn_ 1 $
  force_ (Atom $ L 0) (appbif_ (intrinsicIndex "PRUNE") [L 1])

-- | __DEEPMERGE(l, r)
euDeepMerge :: LambdaForm
euDeepMerge =
  mergeTemplate "__DEEPMERGE" $
  thunkn_ 1 $
  force_
    (Atom $ L 0)
    (appbif_ (intrinsicIndex "PRUNEMERGE") [L 1, gref "DEEPMERGEIFBLOCKS"])

-- | __DEEPMERGEIFBLOCKS
euDeepMergeIfBlocks :: LambdaForm
euDeepMergeIfBlocks =
  lam_ 0 2 $
  ann_ "__DEEPMERGEIFBLOCKS" 0 $
  casedef_
    (Atom $ L 0)
    [ ( stgBlock
      , ( 1
        , casedef_
            (Atom $ L 1)
            [(stgBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))]
            (Atom $ L 1)))
    ]
    (Atom $ L 1)

-- | __BLOCK(l)
euBlock :: LambdaForm
euBlock = lam_ 0 1 $ appcon_ stgBlock [L 0]

-- | __ELEMENTS(b) - return elements in order appropriate for
-- iteration (and pruned so no duplicates...)
euElements :: LambdaForm
euElements =
  lam_ 0 1 $ ann_ "__ELEMENTS" 0 $
  casedef_
    (Atom (L 0))
    [(stgBlock, (1, Atom (L 1)))]
    (appfn_ (gref "PANIC") [V $ NativeString "ELEMENTS expects block"])



-- | __LOOKUP(symbol, block)
euLookup :: LambdaForm
euLookup =
  lam_ 0 2 $
  ann_ "__LOOKUP" 0 $
  case_
    (Atom (L 1))
    [ ( stgBlock
      , ( 1
        , let l = L 2
              reversed = L 3
           in let_ [pc_ [l] $ thunkn_ 1 $ appfn_ (gref "REVERSE") [L 0]] $
              appfn_ (gref "LOOKUPLIST") [reversed, L 0]))
    ]


-- | __LOOKUPOR(symbol, default, block)
euLookupOr :: LambdaForm
euLookupOr =
  let sym = L 0
      deft = L 1
      blk = L 2
   in lam_ 0 3 $
      ann_ "__LOOKUPOR" 0 $
      case_
        (Atom blk)
        [ ( stgBlock
          , ( 1
            , let els = L 2
                  reversed = L 3
               in let_
                    [ pc_ [els] $
                      thunkn_ 1 $ appfn_ (gref "REVERSE") [L 0]
                    ] $
                  appfn_ (gref "LOOKUPLISTOR") [reversed, deft, sym]))
        ]



-- | __LOOKUPLIST(els, key)
euLookupList :: LambdaForm
euLookupList =
  lam_ 0 2 $
  ann_ "__LOOKUPLIST" 0 $
  -- break head off list of elements
  casedef_
    (Atom (L 0))
    [ ( stgCons
      , ( 2
             -- break head (key) of kv pair
        , casedef_
            (Atom (L 2))
            [ ( stgCons
              , ( 2
                  -- compare k with
                , case_
                    (appfn_ (gref "EQ") [L 4, L 1])
                    [ (stgTrue, (0, appfn_ (gref "HEAD") [L 5]))
                    , ( stgFalse
                      , (0, appfn_ (gref "LOOKUPLIST") [L 3, L 1]))
                    ]))
            ]
            (appfn_
               (gref "PANIC")
               [V $ NativeString "Key lookup error (non-pair)."])))
    ]
    (appfn_ (gref "!KEYNOTFOUND") [L 1])


-- | __LOOKUPLISTOR(els, default, key)
euLookupListOr :: LambdaForm
euLookupListOr =
  let l = L 0
      def = L 1
      sym = L 2
   in lam_ 0 3 $
      ann_ "__LOOKUPLISTOR" 0 $
      case_
        (Atom l)
        [ ( stgCons
          , ( 2
            , let h = L 3
                  t = L 4
               in case_
                    (Atom h)
                    [ ( stgCons
                      , ( 2
                        , let k = L 5
                              v = L 6
                           in case_
                                (appfn_ (gref "EQ") [k, sym])
                                [ (stgTrue, (0, appfn_ (gref "HEAD") [v]))
                                , ( stgFalse
                                  , ( 0
                                    , appfn_
                                        (gref "LOOKUPLISTOR")
                                        [t, def, sym]))
                                ]))
                    ]))
        , (stgNil, (1, Atom def))
        ]
