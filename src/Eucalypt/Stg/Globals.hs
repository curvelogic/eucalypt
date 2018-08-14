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

-- | __CAT
cat :: LambdaForm
cat = lam_ 0 2 $ appfn_ (BoundArg 1) [BoundArg 0]

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
standardGlobals = HM.fromList [("CAT", cat), ("seqNatList", seqNatList)]
