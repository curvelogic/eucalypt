{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Eucalypt.Stg.Globals
Description : Standard globals for the STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}
module Eucalypt.Stg.Globals where

import Data.Symbol
import Eucalypt.Stg.GlobalInfo
import qualified Eucalypt.Stg.Globals.Arithmetic as Arith
import qualified Eucalypt.Stg.Globals.Block as Block
import qualified Eucalypt.Stg.Globals.Bool as Bool
import qualified Eucalypt.Stg.Globals.Emit as Emit
import qualified Eucalypt.Stg.Globals.Eq as Eq
import qualified Eucalypt.Stg.Globals.IOHMBlock as IOHM
import qualified Eucalypt.Stg.Globals.IOSMBlock as IOSM
import qualified Eucalypt.Stg.Globals.List as List
import qualified Eucalypt.Stg.Globals.Meta as Meta
import qualified Eucalypt.Stg.Globals.Number as Number
import qualified Eucalypt.Stg.Globals.Panic as Panic
import qualified Eucalypt.Stg.Globals.Str as Str
import qualified Eucalypt.Stg.Globals.Time as Time
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- | Constant: __KNIL
euStgNil :: LambdaForm
euStgNil = nilConstructor

-- | Constant: __KEMPTYBLOCK
euEmptyBlock :: LambdaForm
euEmptyBlock = thunk_ $ appcon_ stgBlock [gref "KNIL"]

-- | __CAT is a "fake" builtin that should be eliminated during core
-- phase and so this should never be called
euCat :: LambdaForm
euCat =
  lam_ 0 2 $
  ann_ "__CAT" 0 $
  appfn_ (gref "PANIC") [V $ NativeString "Uneliminated call to __CAT"]


-- | Strictly evaluate a list of natives to NF
seqNatList :: LambdaForm
seqNatList =
  lam_ 0 1 $
  case_
    (Atom (L 0))
    [ (stgNil, (0, appcon_ stgNil mempty))
    , ( stgCons
      , ( 2
        , force_
            (Atom (L 1))
            (force_
               (appfn_ (gref "seqNatList") [L 2])
               (appcon_ stgCons [L 3, L 4]))))
    ]

-- | Strictly evaluate a list of pairs to NF
seqPairList :: LambdaForm
seqPairList =
  lam_ 0 1 $
  ann_ "__seqPairList" 0 $
  case_
    (Atom (L 0))
    [ (stgNil, (0, appcon_ stgNil []))
    , ( stgCons
      , ( 2
        , let h = L 1
              t = L 2
              et = L 3
              k = L 4
              v = L 5
              ek = L 6
              eh = L 7
           in (force_ (appfn_ (gref "seqPairList") [t]) $
               case_
                 (Atom h)
                 [ ( stgCons
                   , ( 2
                     , force_ (Atom k) $
                       let_ [pc_ [ek, v] $ standardConstructor 2 stgCons] $
                       appcon_ stgCons [eh, et]))
                 ])))
    ]


commonGlobals :: [(Symbol, LambdaForm)]
commonGlobals =
  [ ("CAT", euCat)
  , ("KNIL", euStgNil)
  , ("KEMPTYBLOCK", euEmptyBlock)
  , ("seqNatList", seqNatList)
  , ("seqPairList", seqPairList)
  ]

globals :: [(Symbol, LambdaForm)]
globals =
  concat
    [ commonGlobals
    , Arith.globals
    , Block.globals
    , Bool.globals
    , Emit.globals
    , Eq.globals
    , IOHM.globals
    , IOSM.globals
    , List.globals
    , Meta.globals
    , Number.globals
    , Panic.globals
    , Str.globals
    , Time.globals
    ]
