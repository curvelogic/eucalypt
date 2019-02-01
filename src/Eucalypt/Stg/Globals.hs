{-# LANGUAGE RecordWildCards #-}
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
import Eucalypt.Stg.GlobalInfo
import qualified Eucalypt.Stg.Globals.Arithmetic as Arith
import qualified Eucalypt.Stg.Globals.Block as Block
import qualified Eucalypt.Stg.Globals.Bool as Bool
import qualified Eucalypt.Stg.Globals.Dict as Dict
import qualified Eucalypt.Stg.Globals.Emit as Emit
import qualified Eucalypt.Stg.Globals.Eq as Eq
import qualified Eucalypt.Stg.Globals.List as List
import qualified Eucalypt.Stg.Globals.Meta as Meta
import qualified Eucalypt.Stg.Globals.Number as Number
import qualified Eucalypt.Stg.Globals.Panic as Panic
import qualified Eucalypt.Stg.Globals.Set as Set
import qualified Eucalypt.Stg.Globals.Str as Str
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- | Constant: __KNIL
euStgNil :: LambdaForm
euStgNil = value_ $ appcon_ stgNil mempty

-- | Constant: __KEMPTYBLOCK
euEmptyBlock :: LambdaForm
euEmptyBlock = thunk_ $ appcon_ stgBlock [Global "KNIL"]

-- | __CAT is a "fake" builtin that should be eliminated during core
-- phase and so this should never be called
euCat :: LambdaForm
euCat =
  lam_ 0 2 $
  ann_ "__CAT" 0 $
  appfn_ (Global "PANIC") [Literal $ NativeString "Uneliminated call to __CAT"]


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
  ann_ "__seqPairList" 0 $
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



standardGlobals :: [GlobalInfo]
standardGlobals =
  [ GlobalInfo "EQ" Eq.euEq [NonStrict, NonStrict]
  , GlobalInfo "TRUE" Bool.euTrue []
  , GlobalInfo "FALSE" Bool.euFalse []
  , GlobalInfo "NOT" Bool.euNot [Strict]
  , GlobalInfo "AND" Bool.euAnd [Strict, NonStrict]
  , GlobalInfo "OR" Bool.euOr [Strict, NonStrict]
  , GlobalInfo "IF" Bool.euIf [Strict, NonStrict, NonStrict]
  , GlobalInfo "CONS" List.euCons [NonStrict, NonStrict]
  , GlobalInfo "NIL" List.euNil [NonStrict]
  , GlobalInfo "HEAD" List.euHead [NonStrict]
  , GlobalInfo "TAIL" List.euTail [NonStrict]
  , GlobalInfo "CONCAT" List.euConcat [NonStrict]
  , GlobalInfo "REVERSE" List.euReverse [NonStrict]
  , GlobalInfo "PANIC" Panic.euPanic [Strict]
  , GlobalInfo "!KEYNOTFOUND" Panic.euKeyNotFound [Strict]
  , GlobalInfo "BOMB" Panic.euBomb []
  , GlobalInfo "CAT" euCat [NonStrict, NonStrict]
  , GlobalInfo "BLOCK" Block.euBlock [NonStrict]
  , GlobalInfo "ELEMENTS" Block.euElements [NonStrict]
  , GlobalInfo "MERGE" Block.euMerge [NonStrict, NonStrict]
  , GlobalInfo "DEEPMERGE" Block.euDeepMerge [NonStrict, NonStrict]
  , GlobalInfo
      "DEEPMERGEIFBLOCKS"
      Block.euDeepMergeIfBlocks
      [NonStrict, NonStrict]
  , GlobalInfo "LOOKUP" Block.euLookup [NonStrict, Strict]
  , GlobalInfo "LOOKUPLIST" Block.euLookupList [NonStrict, Strict]
  , GlobalInfo "LOOKUPOR" Block.euLookupOr [NonStrict, Strict, NonStrict]
  , GlobalInfo
      "LOOKUPLISTOR"
      Block.euLookupListOr
      [NonStrict, Strict, NonStrict]
  , GlobalInfo "META" Meta.euMeta [Strict]
  , GlobalInfo "WITHMETA" Meta.euWithMeta [NonStrict, NonStrict]
  , GlobalInfo "KNIL" euStgNil []
  , GlobalInfo "KEMPTYBLOCK" euEmptyBlock []
  , GlobalInfo "seqNatList" seqNatList [NonStrict]
  , GlobalInfo "seqPairList" seqPairList [NonStrict]
  ] <>
  Arith.globals <>
  Emit.globals <>
  Number.globals <>
  Str.globals <>
  Set.globals <>
  Dict.globals

standardGlobalMap :: HM.HashMap String LambdaForm
standardGlobalMap = HM.fromList $ map toPair standardGlobals
  where
    toPair GlobalInfo{..} = (globalName, globalCode)

standardGlobalInfoMap :: HM.HashMap String GlobalInfo
standardGlobalInfoMap = HM.fromList $ map toPair standardGlobals
  where
    toPair g@GlobalInfo{..} = (globalName, g)

standardGlobalStrictness :: String -> [Strictness]
standardGlobalStrictness n = globalStrictness $ standardGlobalInfoMap HM.! n
