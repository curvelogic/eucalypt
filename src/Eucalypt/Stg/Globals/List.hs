{-|
Module      : Eucalypt.Stg.Globals.List
Description : Standard list globals for the STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Globals.List
  ( euConcat
  , euCons
  , euNil
  , euHead
  , euTail
  , euReverse
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags


-- | __CONS(h, t)
euCons :: LambdaForm
euCons = lam_ 0 2 $ appcon_ stgCons [Local 0, Local 1]



-- | __NIL
euNil :: LambdaForm
euNil =
  lam_ 0 1 $
  ann_ "NIL" 0 $
  case_
    (Atom (Local 0))
    [ (stgCons, (2, Atom (Literal (NativeBool False))))
    , (stgNil, (0, Atom (Literal (NativeBool True))))
    ]



-- | __HEAD(list)
euHead :: LambdaForm
euHead =
  lam_ 0 1 $
  ann_ "__HEAD" 0 $
  case_
    (Atom (Local 0))
    [ (stgCons, (2, Atom (Local 1)))
    , ( stgNil
      , ( 0
        , appfn_ (Global "PANIC") [Literal $ NativeString "Head of empty list"]))
    ]



-- | __TAIL(list)
euTail :: LambdaForm
euTail =
  lam_ 0 1 $ ann_ "__TAIL" 0 $
  case_ (Atom (Local 0)) [(stgCons, (2, Atom (Local 2)))]


-- | __CONCAT(l, r)
euConcat :: LambdaForm
euConcat =
  let l = Local 0
      r = Local 1
   in lam_ 0 2 $
      ann_ "__CONCAT" 0 $
      case_
        (Atom l)
        [ ( stgCons
          , ( 2
            , let h = Local 2
                  t = Local 3
                  recur = Local 4
               in let_
                    [ pc_ [t, r] $
                      value_ $ appfn_ (Global "CONCAT") [Local 0, Local 1]
                    ]
                    (appcon_ stgCons [h, recur])))
        , (stgNil, (0, Atom r))
        ]


-- | __REVERSE(l)
euReverse :: LambdaForm
euReverse =
  lam_ 0 1 $ ann_ "__REVERSE" 0 $
  let list = Local 0
      self = Local 1
      empty = Local 2
   in letrec_
        [ pc_ [self] $
          lam_ 1 2 $
          let recurse = Local 0
              acc = Local 1
              rest = Local 2
           in casedef_
                 (Atom rest)
                 [ ( stgCons
                   , ( 2
                     , let h = Local 3
                           t = Local 4
                           newacc = Local 5
                        in let_
                             [pc_ [h, acc] $ standardConstructor 2 stgCons]
                             (appfn_ recurse [newacc, t])))
                 , (stgNil, (0, Atom acc))
                 ]
                 (appfn_ (Global "PANIC") [Literal $ NativeString "Improper list in __REVERSE"])
        , pc0_ $ standardConstructor 0 stgNil
        ]
        (appfn_ self [empty, list])
