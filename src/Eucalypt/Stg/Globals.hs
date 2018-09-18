{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Eucalypt.Stg.Globals
Description : Standard globals for the STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}
module Eucalypt.Stg.Globals where

import qualified Data.HashMap.Strict as HM
import Eucalypt.Stg.Globals.Arithmetic as Arith
import Eucalypt.Stg.Globals.Block as Block
import Eucalypt.Stg.Globals.Bool as Bool
import Eucalypt.Stg.Globals.Emit as Emit
import Eucalypt.Stg.Globals.Eq as Eq
import Eucalypt.Stg.Globals.List as List
import Eucalypt.Stg.Globals.Meta as Meta
import Eucalypt.Stg.Globals.Panic as Panic
import Eucalypt.Stg.Globals.Str as Str
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- | Constant: __KNIL
euStgNil :: LambdaForm
euStgNil = value_ $ appcon_ stgNil mempty

-- | Constant: __KEMPTYBLOCK
euEmptyBlock :: LambdaForm
euEmptyBlock = thunk_ $ appcon_ stgBlock [Global "KNIL"]

-- | __CAT(x, f)
euCat :: LambdaForm
euCat =
  lam_ 0 2 $
  ann_ "__CAT" $
  casedef_
    (Atom (Local 1))
    [(stgBlock, (1, appfn_ (Global "MERGE") [Local 0, Local 1]))] $
  appfn_ (Local 1) [Local 0]


-- | Strictly evaluate a list of natives to NF
seqNatList :: LambdaForm
seqNatList =
  lam_ 0 1 $
  case_
    (Atom (Local 0))
    [ (stgNil, (0, appcon_ stgNil mempty))
    , ( stgCons
      , ( 2
        , force_
            (Atom (Local 1))
            (force_
               (appfn_ (Global "seqNatList") [Local 2])
               (appcon_ stgCons [Local 3, Local 4]))))
    ]

-- | Strictly evaluate a list of pairs to NF
seqPairList :: LambdaForm
seqPairList =
  lam_ 0 1 $
  ann_ "__seqPairList" $
  case_
    (Atom (Local 0))
    [ (stgNil, (0, appcon_ stgNil []))
    , ( stgCons
      , ( 2
        , let h = Local 1
              t = Local 2
              et = Local 3
              k = Local 4
              v = Local 5
              ek = Local 6
              eh = Local 7
           in (force_ (appfn_ (Global "seqPairList") [t]) $
               case_
                 (Atom h)
                 [ ( stgCons
                   , ( 2
                     , force_ (Atom k) $
                       let_ [pc_ [ek, v] $ standardConstructor 2 stgCons] $
                       appcon_ stgCons [eh, et]))
                 ])))
    ]


standardGlobals :: HM.HashMap String LambdaForm
standardGlobals =
  HM.fromList $
  [ ("EQ", Eq.euEq)
  , ("TRUE", Bool.euTrue)
  , ("FALSE", Bool.euFalse)
  , ("NOT", Bool.euNot)
  , ("AND", Bool.euAnd)
  , ("OR", Bool.euOr)
  , ("IF", Bool.euIf)
  , ("CONS", List.euCons)
  , ("NIL", List.euNil)
  , ("HEAD", List.euHead)
  , ("TAIL", List.euTail)
  , ("CONCAT", List.euConcat)
  , ("REVERSE", List.euReverse)
  , ("ADD", Arith.euAdd)
  , ("SUB", Arith.euSub)
  , ("MUL", Arith.euMul)
  , ("DIV", Arith.euDiv)
  , ("LT", Arith.euLt)
  , ("GT", Arith.euGt)
  , ("LTE", Arith.euLte)
  , ("GTE", Arith.euGte)
  , ("PANIC", Panic.euPanic)
  , ("BOMB", Panic.euBomb)
  , ("MATCHES", Str.euMatches)
  , ("MATCH", Str.euMatch)
  , ("JOIN", Str.euJoin)
  , ("SPLIT", Str.euSplit)
  , ("STR", Str.euStr)
  , ("SYM", Str.euSym)
  , ("CAT", euCat)
  , ("HEAD", euHead)
  , ("BLOCK", Block.euBlock)
  , ("ELEMENTS", Block.euElements)
  , ("MERGE", Block.euMerge)
  , ("DEEPMERGE", Block.euDeepMerge)
  , ("DEEPMERGEIFBLOCKS", Block.euDeepMergeIfBlocks)
  , ("LOOKUP", Block.euLookup)
  , ("LOOKUPLIST", Block.euLookupList)
  , ("LOOKUPOR", Block.euLookupOr)
  , ("LOOKUPLISTOR", Block.euLookupListOr)
  , ("META", Meta.euMeta)
  , ("WITHMETA", Meta.euWithMeta)
  , ("KNIL", euStgNil)
  , ("KEMPTYBLOCK", euEmptyBlock)
  , ("seqNatList", seqNatList)
  , ("seqPairList", seqPairList)
  ] <>
  Emit.globals
