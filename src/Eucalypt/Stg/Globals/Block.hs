{-# LANGUAGE OverloadedStrings #-}

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

import Data.Symbol
import Eucalypt.Stg.GlobalInfo (gref)
import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

globals :: [(Symbol, LambdaForm)]
globals =
  [ ("BLOCK", euBlock)
  , ("ELEMENTS", euElements)
  , ("MERGE", euMerge)
  , ("ALIST.MERGE", euAListMerge)
  , ("ALIST.PRUNE", euAListPrune)
  , ("ALIST.DEEPMERGE", euAListDeepMerge)
  , ("DEEPMERGE", euDeepMerge)
  , ("DEEPMERGEIFBLOCKS", euDeepMergeIfBlocks)
  , ("LOOKUP", euLookup)
  , ("LOOKUPLIST", euLookupList)
  , ("LOOKUPOR", euLookupOr)
  , ("LOOKUPLISTOR", euLookupListOr)
  ]


-- | Polymorphic block merge
euMerge :: LambdaForm
euMerge =
  lam_ 0 2 $
  ann_ "__ALIST.MERGE" 0 $
  case_
    (Atom $ L 0)
    [ ( stgBlock
      , ( 1
        , case_
            (Atom $ L 1)
            [ ( stgBlock
              , ( 1
                , force_ (appfn_ (gref "ALIST.MERGE") [L 2, L 3]) $
                  appcon_ stgBlock [L 4]))
            , ( stgIOHMBlock
              , ( 1
                , force_ (appfn_ (gref "ELEMENTS") [L 1]) $
                  force_ (appfn_ (gref "ALIST.MERGE") [L 2, L 4]) $
                  appfn_ (gref "IOHM.WRAP") [L 5]))
            , ( stgIOSMBlock
              , ( 1
                , force_ (appfn_ (gref "ELEMENTS") [L 1]) $
                  force_ (appfn_ (gref "ALIST.MERGE") [L 2, L 4]) $
                  appfn_ (gref "IOSM.WRAP") [L 5]))
            ]))
    , ( stgIOHMBlock
      , ( 1
        , case_
            (Atom $ L 1)
            [ ( stgBlock
              , ( 1
                , force_ (appfn_ (gref "IOHM.ELEMENTS") [L 2]) $
                  force_ (appfn_ (gref "ALIST.MERGE") [L 4, L 3]) $
                  appcon_ stgBlock [L 5]))
            , (stgIOHMBlock, (1, appfn_ (gref "IOHM.MERGE") [L 2, L 3]))
            , ( stgIOSMBlock
              , ( 1
                , force_ (appfn_ (gref "IOHM.ELEMENTS") [L 2]) $
                  force_ (appfn_ (gref "IOSM.ELEMENTS") [L 3]) $
                  force_ (appfn_ (gref "ALIST.MERGE") [L 4, L 5]) $
                  appcon_ stgBlock [L 6]))
            ]))
    , ( stgIOHMBlock
      , ( 1
        , case_
            (Atom $ L 1)
            [ ( stgBlock
              , ( 1
                , force_ (appfn_ (gref "IOSM.ELEMENTS") [L 2]) $
                  force_ (appfn_ (gref "ALIST.MERGE") [L 4, L 3]) $
                  appcon_ stgBlock [L 5]))
            , ( stgIOHMBlock
              , ( 1
                , force_ (appfn_ (gref "IOSM.ELEMENTS") [L 2]) $
                  force_ (appfn_ (gref "IOHM.ELEMENTS") [L 3]) $
                  force_ (appfn_ (gref "ALIST.MERGE") [L 4, L 5]) $
                  appcon_ stgBlock [L 6]))
            , (stgIOSMBlock, (1, appfn_ (gref "IOSM.MERGE") [L 2, L 3]))
            ]))
    ]

-- | Merge two association lists
euAListMerge :: LambdaForm
euAListMerge =
  lam_ 0 2 $
  ann_ "__ALIST.MERGE" 0 $
  letrec_ [pc_ [L 0, L 1] $ valuen_ 2 $ appfn_ (gref "CONCAT") [L 0, L 1]] $
  appfn_ (gref "ALIST.PRUNE") [L 2]

euAListPrune :: LambdaForm
euAListPrune =
  lam_ 0 1 $
  ann_ "__ALIST.PRUNE" 0 $
  force_ (appfn_ (gref "seqPairList") [L 0]) $
  appbif_ (intrinsicIndex "PRUNE") [L 1]

euDeepMerge :: LambdaForm
euDeepMerge =
  lam_ 0 2 $
  ann_ "__DEEPMERGE" 0 $
  case_
    (Atom $ L 0)
    [ ( stgBlock
      , ( 1
        , case_
            (Atom $ L 1)
            [ ( stgBlock
              , ( 1
                , force_ (appfn_ (gref "ALIST.DEEPMERGE") [L 2, L 3]) $
                  appcon_ stgBlock [L 4]))
            , ( stgIOHMBlock
              , ( 1
                , force_ (appfn_ (gref "ELEMENTS") [L 1]) $
                  force_ (appfn_ (gref "ALIST.DEEPDEEPMERGE") [L 2, L 4]) $
                  appfn_ (gref "IOHM.WRAP") [L 5]))
            , ( stgIOSMBlock
              , ( 1
                , force_ (appfn_ (gref "ELEMENTS") [L 1]) $
                  force_ (appfn_ (gref "ALIST.DEEPMERGE") [L 2, L 4]) $
                  appfn_ (gref "IOSM.WRAP") [L 5]))
            ]))
    , ( stgIOHMBlock
      , ( 1
        , case_
            (Atom $ L 1)
            [ ( stgBlock
              , ( 1
                , force_ (appfn_ (gref "IOHM.ELEMENTS") [L 2]) $
                  force_ (appfn_ (gref "ALIST.DEEPMERGE") [L 4, L 3]) $
                  appcon_ stgBlock [L 5]))
            , (stgIOHMBlock, (1, appfn_ (gref "IOHMBLOCK.DEEPMERGE") [L 0, L 1]))
            , ( stgIOSMBlock
              , ( 1
                , force_ (appfn_ (gref "IOHM.ELEMENTS") [L 2]) $
                  force_ (appfn_ (gref "IOSM.ELEMENTS") [L 3]) $
                  force_ (appfn_ (gref "ALIST.DEEPMERGE") [L 4, L 5]) $
                  appcon_ stgBlock [L 6]))
            ]))
    , ( stgIOHMBlock
      , ( 1
        , case_
            (Atom $ L 1)
            [ ( stgBlock
              , ( 1
                , force_ (appfn_ (gref "IOSM.ELEMENTS") [L 2]) $
                  force_ (appfn_ (gref "ALIST.DEEPMERGE") [L 4, L 3]) $
                  appcon_ stgBlock [L 5]))
            , ( stgIOHMBlock
              , ( 1
                , force_ (appfn_ (gref "IOSM.ELEMENTS") [L 2]) $
                  force_ (appfn_ (gref "IOHM.ELEMENTS") [L 3]) $
                  force_ (appfn_ (gref "ALIST.DEEPMERGE") [L 4, L 5]) $
                  appcon_ stgBlock [L 6]))
            , (stgIOSMBlock, (1, appfn_ (gref "IOSMBLOCK.DEEPMERGE") [L 0, L 1]))
            ]))
    ]

-- | __DEEPMERGE(l, r)
euAListDeepMerge :: LambdaForm
euAListDeepMerge =
  lam_ 0 2 $
  ann_ "ALIST.DEEPMERGE" 0 $
  letrec_
    [ pc_ [L 0, L 1] $ valuen_ 2 $ appfn_ (gref "CONCAT") [L 0, L 1]
    , pc_ [L 2] $ valuen_ 1 $ appfn_ (gref "seqPairList") [L 0]
    ] $
  force_
    (Atom $ L 3)
    (appbif_ (intrinsicIndex "PRUNEMERGE") [L 4, gref "DEEPMERGEIFBLOCKS"])


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
            [ (stgBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            , (stgIOHMBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            , (stgIOSMBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            ]
            (Atom $ L 1)))
    , ( stgIOHMBlock
      , ( 1
        , casedef_
            (Atom $ L 1)
            [ (stgBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            , (stgIOHMBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            , (stgIOSMBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            ]
            (Atom $ L 1)))
    , ( stgIOSMBlock
      , ( 1
        , casedef_
            (Atom $ L 1)
            [ (stgBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            , (stgIOHMBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            , (stgIOSMBlock, (1, appfn_ (gref "DEEPMERGE") [L 0, L 1]))
            ]
            (Atom $ L 1)))
    ]
    (Atom $ L 1)


-- | __BLOCK(l)
euBlock :: LambdaForm
euBlock = lam_ 0 1 $ appcon_ stgBlock [L 0]


euElements :: LambdaForm
euElements =
  lam_ 0 1 $
  ann_ "__ELEMENTS" 0 $
  casedef_
    (Atom $ L 0)
    [ (stgBlock, (1, appfn_ (gref "ALIST.PRUNE") [L 1]))
    , (stgIOHMBlock, (1, appfn_ (gref "IOHM.ELEMENTS") [L 1]))
    , (stgIOSMBlock, (1, appfn_ (gref "IOSM.ELEMENTS") [L 1]))
    ]
    (appfn_ (gref "PANIC") [V $ NativeString "ELEMENTS expects block"])


-- | __LOOKUP(symbol, block)
--
-- TEMP: Make polymorphic to IOHM / IOSM blocks too
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
    , (stgIOHMBlock, (1, appfn_ (gref "IOHM.LOOKUP") [L 2, L 0]))
    , (stgIOSMBlock, (1, appfn_ (gref "IOSM.LOOKUP") [L 2, L 0]))
    ]


-- | __LOOKUPOR(symbol, default, block)
--
-- TEMP: Make polymorphic to IOHM / IOSM blocks too
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
               in let_ [pc_ [els] $ thunkn_ 1 $ appfn_ (gref "REVERSE") [L 0]] $
                  appfn_ (gref "LOOKUPLISTOR") [reversed, deft, sym]))
        , (stgIOHMBlock, (1, appfn_ (gref "IOHM.LOOKUPOR") [L 3, L 0, L 1]))
        , (stgIOSMBlock, (1, appfn_ (gref "IOSM.LOOKUPOR") [L 3, L 0, L 1]))
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
