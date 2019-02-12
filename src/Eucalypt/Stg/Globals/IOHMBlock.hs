{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.IOHMBlock
Description : Functions for dealing with IOHMBlocks
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.IOHMBlock
  ( globals
  ) where

import Data.Symbol
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Globals.Common
import Eucalypt.Stg.Intrinsics
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

globals :: [(Symbol, LambdaForm)]
globals =
  [ ("IOHM.EMPTY", euIOHMEmpty)
  , ("IOHM.INSERT", euIOHMInsert)
  , ("IOHM.WRAP", euIOHMWrap)
  , ("IOHM.LIST", euIOHMList)
  , ("IOHM.LOOKUP", euIOHMLookup)
  , ("IOHM.LOOKUPOR", euIOHMLookupOr)
  , ("IOHM.UNWRAP", euIOHMUnwrap)
  , ("IOHM.MERGE", euIOHMMerge)
  , ("IOHM.MERGEWITH", euIOHMMergeWith)
  , ("IOHMBLOCK.DEEPMERGE", euDeepMerge)
  , ("IOHMBLOCK.DEEPMERGEIFBLOCKS", euDeepMergeIfBlocks)
  , ("IOHM.EQ", euIOHMEq)
  ]

euIOHMEmpty :: LambdaForm
euIOHMEmpty = wrapBif "IOHM.EMPTY"

euIOHMInsert :: LambdaForm
euIOHMInsert = wrapBifStrict "IOHM.INSERT"

euIOHMWrap :: LambdaForm
euIOHMWrap = lam_ 0 1 $ ann_ "IOHM.WRAP" 0 $ appcon_ stgIOHMBlock [L 0]

euIOHMList :: LambdaForm
euIOHMList = wrapBifStrict "IOHM.LIST"

euIOHMLookup :: LambdaForm
euIOHMLookup = wrapBifStrict "IOHM.LOOKUP"

-- Lazy in default argument
euIOHMLookupOr :: LambdaForm
euIOHMLookupOr =
  lam_ 0 3 $
  ann_ "IOHM.LOOKUPOR" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $ appbif_ (intrinsicIndex "IOHM.LOOKUPOR") [L 3, L 4, L 2]

euIOHMUnwrap :: LambdaForm
euIOHMUnwrap =
  lam_ 0 1 $
  ann_ "IOHM.UNWRAP" 0 $
  casedef_ (Atom (L 0)) [(stgIOHMBlock, (1, atom_ (L 1)))] $
  appfn_ (gref "PANIC") [V $ NativeString "Expected IOHMBlock"]

euIOHMMerge :: LambdaForm
euIOHMMerge = wrapBifStrict "IOHM.MERGE"

euIOHMMergeWith :: LambdaForm
euIOHMMergeWith = wrapBifStrict "IOHM.MERGEWITH"

euDeepMerge :: LambdaForm
euDeepMerge =
  lam_ 0 2 $
  ann_ "IOHMBLOCK.DEEPMERGE" 0 $
  casedef_
    (Atom $ L 0)
    [ ( stgIOHMBlock
      , ( 1
        , casedef_
            (Atom $ L 1)
            [ ( stgIOHMBlock
              , ( 1
                , let_
                    [ pc_ [L 2, L 3, gref "IOHMBLOCK.DEEPMERGEIFBLOCKS"] $
                      thunkn_ 3 $ appfn_ (gref "IOHM.MERGEWITH") [L 0, L 1, L 2]
                    ] $
                  appcon_ stgIOHMBlock [L 4]))
            ]
            (appfn_
               (gref "PANIC")
               [V $ NativeString "Non block argument to deep merge"])))
    ]
    (appfn_ (gref "PANIC") [V $ NativeString "Non block argument to deep merge"])

euDeepMergeIfBlocks :: LambdaForm
euDeepMergeIfBlocks =
  lam_ 0 2 $
  ann_ "IOHMBLOCK.DEEPMERGEIFBLOCKS" 0 $
  casedef_
    (Atom $ L 0)
    [ ( stgIOHMBlock
      , ( 1
        , casedef_
            (Atom $ L 1)
            [(stgIOHMBlock, (1, appfn_ (gref "IOHMBLOCK.DEEPMERGE") [L 0, L 1]))]
            (Atom $ L 1)))
    ]
    (Atom $ L 1)

euIOHMEq :: LambdaForm
euIOHMEq =
  lam_ 0 2 $
  ann_ "IOHM.EQ" 0 $
  force_ (appfn_ (gref "IOHM.LIST") [L 0]) $
  force_ (appfn_ (gref "IOHM.LIST") [L 1]) $ appfn_ (gref "EQ") [L 2, L 3]
