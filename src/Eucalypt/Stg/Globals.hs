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
import Eucalypt.Stg.Globals.Bool as Bool
import Eucalypt.Stg.Globals.Emit as Emit
import Eucalypt.Stg.Globals.Eq as Eq
import Eucalypt.Stg.Globals.List as List
import Eucalypt.Stg.Globals.Panic as Panic
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- | __CAT(x, f)
euCat :: LambdaForm
euCat = lam_ 0 2 $ appfn_ (BoundArg 1) [BoundArg 0]


-- | __LOOKUP(block, symbol)
euLookup :: LambdaForm
euLookup =
  lam_ 0 2 $ ann_ "__LOOKUP" $
  case_
    (Atom (BoundArg 0))
    [(stgBlock, (1, appfn_ (Global "LOOKUP_LIST") [Local 2, BoundArg 1]))]



-- | _LOOKUP_LIST(els, key)
euLookupList :: LambdaForm
euLookupList =
  lam_ 0 2 $ ann_ "__LOOKUP_LIST" $
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
                      , appfn_ (Global "LOOKUP_LIST") [Local 3, BoundArg 1])
                    ]
                    Nothing))
            ]))
    ]



-- | Strictly evaluate a list of natives to NF
seqNatList :: LambdaForm
seqNatList =
  lam_ 0 1 $
  case_
    (Atom (BoundArg 0))
    [ (stgNil, (0, appcon_ stgNil mempty))
    , ( stgCons
      , ( 2
        , force_
            (Atom (Local 1)) -- boundarg becomes 0... :(
            (force_
               (appfn_ (Global "seqNatList") [Local 2])
               (appcon_ stgCons [Local 3, Local 4]))))
    ]

standardGlobals :: HM.HashMap String LambdaForm
standardGlobals =
  HM.fromList
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
    , ("CAT", euCat)
    , ("HEAD", euHead)
    , ("LOOKUP", euLookup)
    , ("LOOKUP_LIST", euLookupList)
    , ("RENDER", Emit.euRender)
    , ("NULL", Emit.euNull)
    , ("seqNatList", seqNatList)
    ]
