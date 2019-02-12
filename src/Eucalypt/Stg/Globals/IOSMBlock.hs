{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.IOSMBlock
Description : Functions for dealing with IOSMBlocks
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.IOSMBlock
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
  [ ("IOSM.EMPTY", euIOSMEmpty)
  , ("IOSM.INSERT", euIOSMInsert)
  , ("IOSM.WRAP", euIOSMWrap)
  , ("IOSM.LIST", euIOSMList)
  , ("IOSM.LOOKUP", euIOSMLookup)
  , ("IOSM.LOOKUPOR", euIOSMLookupOr)
  , ("IOSM.UNWRAP", euIOSMUnwrap)
  , ("IOSM.MERGE", euIOSMMerge)
  , ("IOSM.MERGEWITH", euIOSMMergeWith)
  , ("IOSMBLOCK.DEEPMERGE", euDeepMerge)
  , ("IOSMBLOCK.DEEPMERGEIFBLOCKS", euDeepMergeIfBlocks)
  , ("IOSM.EQ", euIOSMEq)
  ]

euIOSMEmpty :: LambdaForm
euIOSMEmpty = wrapBif "IOSM.EMPTY"

euIOSMInsert :: LambdaForm
euIOSMInsert = wrapBifStrict "IOSM.INSERT"

euIOSMWrap :: LambdaForm
euIOSMWrap = lam_ 0 1 $ ann_ "IOSM.WRAP" 0 $ appcon_ stgIOSMBlock [L 0]

euIOSMList :: LambdaForm
euIOSMList = wrapBifStrict "IOSM.LIST"

euIOSMLookup :: LambdaForm
euIOSMLookup = wrapBifStrict "IOSM.LOOKUP"

-- Lazy in default argument
euIOSMLookupOr :: LambdaForm
euIOSMLookupOr =
  lam_ 0 3 $
  ann_ "IOSM.LOOKUPOR" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $ appbif_ (intrinsicIndex "IOSM.LOOKUPOR") [L 3, L 4, L 2]

euIOSMUnwrap :: LambdaForm
euIOSMUnwrap =
  lam_ 0 1 $
  ann_ "IOSM.UNWRAP" 0 $
  casedef_ (Atom (L 0)) [(stgIOSMBlock, (1, atom_ (L 1)))] $
  appfn_ (gref "PANIC") [V $ NativeString "Expected IOSMBlock"]

euIOSMMerge :: LambdaForm
euIOSMMerge = wrapBifStrict "IOSM.MERGE"

euIOSMMergeWith :: LambdaForm
euIOSMMergeWith = wrapBifStrict "IOSM.MERGEWITH"

euDeepMerge :: LambdaForm
euDeepMerge =
  lam_ 0 2 $
  ann_ "IOSMBLOCK.DEEPMERGE" 0 $
  casedef_
    (Atom $ L 0)
    [ ( stgIOSMBlock
      , ( 1
        , casedef_
            (Atom $ L 1)
            [ ( stgIOSMBlock
              , ( 1
                , let_
                    [ pc_ [L 2, L 3, gref "IOSMBLOCK.DEEPMERGEIFBLOCKS"] $
                      thunkn_ 3 $ appfn_ (gref "IOSM.MERGEWITH") [L 0, L 1, L 2]
                    ] $
                  appcon_ stgIOSMBlock [L 4]))
            ]
            (appfn_
               (gref "PANIC")
               [V $ NativeString "Non block argument to deep merge"])))
    ]
    (appfn_ (gref "PANIC") [V $ NativeString "Non block argument to deep merge"])

euDeepMergeIfBlocks :: LambdaForm
euDeepMergeIfBlocks =
  lam_ 0 2 $
  ann_ "IOSMBLOCK.DEEPMERGEIFBLOCKS" 0 $
  casedef_
    (Atom $ L 0)
    [ ( stgIOSMBlock
      , ( 1
        , casedef_
            (Atom $ L 1)
            [(stgIOSMBlock, (1, appfn_ (gref "IOSMBLOCK.DEEPMERGE") [L 0, L 1]))]
            (Atom $ L 1)))
    ]
    (Atom $ L 1)

euIOSMEq :: LambdaForm
euIOSMEq =
  lam_ 0 2 $
  ann_ "IOSM.EQ" 0 $
  force_ (appfn_ (gref "IOSM.LIST") [L 0]) $
  force_ (appfn_ (gref "IOSM.LIST") [L 1]) $ appfn_ (gref "EQ") [L 2, L 3]
