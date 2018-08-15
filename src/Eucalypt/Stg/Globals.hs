{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Eucalypt.Stg.Globas
Description : Standard globals for the STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

These are STG snippets, mainly for marshalling values between haskel
world and the machine world, and for forcing strict evaluation etc.

-}
module Eucalypt.Stg.Globals where

import qualified Data.HashMap.Strict as HM
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- | __CAT(x, f)
euCat :: LambdaForm
euCat = lam_ 0 2 $ appfn_ (BoundArg 1) [BoundArg 0]


-- | __HEAD(list)
euHead :: LambdaForm
euHead =
  lam_ 0 1 $
  case_ (Atom (BoundArg 0)) [(stgCons, (2, Atom (Local 1)))] -- binds after


-- | __LOOKUP(block, symbol)
euLookup :: LambdaForm
euLookup =
  lam_ 0 2 $
  case_
    (Atom (BoundArg 0))
    [(stgBlock, (1, appfn_ (Global "LOOKUP_LIST") [Local 2, BoundArg 1]))]



-- | _LOOKUP_LIST(els, key)
euLookupList :: LambdaForm
euLookupList =
  lam_ 0 2 $
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
                    (appfn_ (Global "EQ") [Local 3, BoundArg 1])
                    [ (NativeBool True, appfn_ (Global "HEAD") [Local 4])
                    , ( NativeBool False
                      , appfn_ (Global "LOOKUP_LIST") [Local 4, BoundArg 1])
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
        , forcelit_
            (Atom (Local 1)) -- boundarg becomes 0... :(
            (force_
               (appfn_ (Global "seqNatList") [Local 2])
               (appcon_ stgCons [Local 3, Local 4]))))
    ]

standardGlobals :: HM.HashMap String LambdaForm
standardGlobals =
  HM.fromList
    [ ("CAT", euCat)
    , ("HEAD", euHead)
    , ("LOOKUP", euLookup)
    , ("LOOKUP_LIST", euLookupList)
    , ("seqNatList", seqNatList)
    ]
