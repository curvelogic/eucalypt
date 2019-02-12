{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Eucalypt.Stg.Globals.List
Description : Standard list globals for the STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Globals.List
  ( globals
  ) where

import Data.Symbol
import Eucalypt.Stg.GlobalInfo (gref)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags


globals :: [(Symbol, LambdaForm)]
globals =
  [ ("CONS", euCons)
  , ("NIL", euNil)
  , ("HEAD", euHead)
  , ("TAIL", euTail)
  , ("CONCAT", euConcat)
  , ("REVERSE", euReverse)
  ]


-- | __CONS(h, t)
euCons :: LambdaForm
euCons = lam_ 0 2 $ appcon_ stgCons [L 0, L 1]



-- | __NIL
euNil :: LambdaForm
euNil =
  lam_ 0 1 $
  ann_ "NIL" 0 $
  case_
    (Atom (L 0))
    [ (stgCons, (2, appcon_ stgFalse []))
    , (stgNil, (0, appcon_ stgTrue []))
    ]



-- | __HEAD(list)
euHead :: LambdaForm
euHead =
  lam_ 0 1 $
  ann_ "__HEAD" 0 $
  case_
    (Atom (L 0))
    [ (stgCons, (2, Atom (L 1)))
    , ( stgNil
      , (0, appfn_ (gref "PANIC") [V $ NativeString "Head of empty list"]))
    ]



-- | __TAIL(list)
euTail :: LambdaForm
euTail =
  lam_ 0 1 $ ann_ "__TAIL" 0 $
  case_ (Atom (L 0)) [(stgCons, (2, Atom (L 2)))]


-- | __CONCAT(l, r)
euConcat :: LambdaForm
euConcat =
  let l = L 0
      r = L 1
   in lam_ 0 2 $
      ann_ "__CONCAT" 0 $
      case_
        (Atom l)
        [ ( stgCons
          , ( 2
            , let h = L 2
                  t = L 3
                  recur = L 4
               in let_
                    [ pc_ [t, r] $
                      value_ $ appfn_ (gref "CONCAT") [L 0, L 1]
                    ]
                    (appcon_ stgCons [h, recur])))
        , (stgNil, (0, Atom r))
        ]


-- | __REVERSE(l)
euReverse :: LambdaForm
euReverse =
  lam_ 0 1 $ ann_ "__REVERSE" 0 $
  let list = L 0
      self = L 1
      empty = L 2
   in letrec_
        [ pc_ [self] $
          lam_ 1 2 $
          let recurse = L 0
              acc = L 1
              rest = L 2
           in casedef_
                 (Atom rest)
                 [ ( stgCons
                   , ( 2
                     , let h = L 3
                           t = L 4
                           newacc = L 5
                        in let_
                             [pc_ [h, acc] $ standardConstructor 2 stgCons]
                             (appfn_ recurse [newacc, t])))
                 , (stgNil, (0, Atom acc))
                 ]
                 (appfn_ (gref "PANIC") [V $ NativeString "Improper list in __REVERSE"])
        , pc0_ $ standardConstructor 0 stgNil
        ]
        (appfn_ self [empty, list])
