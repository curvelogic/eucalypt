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
  , ("IOSM.FROMLIST", euIOSMFromList)
  , ("IOSM.LOOKUP", euIOSMLookup)
  , ("IOSM.LOOKUPOR", euIOSMLookupOr)
  , ("IOSM.UNWRAP", euIOSMUnwrap)
  , ("IOSM.MERGE", euIOSMMerge)
  , ("IOSM.DEEPMERGE", euIOSMDeepMerge)
  , ("IOSM.MERGEWITH", euIOSMMergeWith)
  , ("IOSMBLOCK.DEEPMERGE", euDeepMerge)
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

euIOSMFromList :: LambdaForm
euIOSMFromList =
  lam_ 0 1 $
  ann_ "__IOSM.FROMLIST" 0 $
  force_ (appfn_ (gref "seqPairList") [L 0]) $
  appbif_ (intrinsicIndex "IOSM.FROMALIST") [L 1]


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

euIOSMDeepMerge :: LambdaForm
euIOSMDeepMerge =
  lam_ 0 2 $
  ann_ "IOSM.DEEPMERGE" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $
  appfn_ (gref "IOSM.MERGEWITH") [L 2, L 3, gref "DEEPMERGEIFBLOCKS"]

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
                , force_ (appfn_ (gref "IOSM.DEEPMERGE") [L 2, L 3]) $
                  appcon_ stgIOSMBlock [L 4]))
            ]
            (appfn_
               (gref "PANIC")
               [V $ NativeString "Non block argument to deep merge"])))
    ]
    (appfn_ (gref "PANIC") [V $ NativeString "Non block argument to deep merge"])

euIOSMEq :: LambdaForm
euIOSMEq =
  lam_ 0 2 $
  ann_ "IOSM.EQ" 0 $
  force_ (Atom $ L 0) $
  force_ (Atom $ L 1) $
  force_ (appfn_ (gref "IOSM.LIST") [L 2]) $
  force_ (appfn_ (gref "IOSM.LIST") [L 3]) $ appfn_ (gref "EQ") [L 4, L 5]
