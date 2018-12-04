{-|
Module      : Eucalypt.Stg.Globals.Str
Description : String and regex globals for STG implementation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}

module Eucalypt.Stg.Globals.Str
  ( globals
  ) where

import Eucalypt.Stg.Syn
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [GlobalInfo]
globals =
  [ GlobalInfo "MATCHES" euMatches [Strict, Strict]
  , GlobalInfo "MATCH" euMatch [Strict, Strict]
  , GlobalInfo "JOIN" euJoin [Strict, Strict]
  , GlobalInfo "SPLIT" euSplit [Strict, Strict]
  , GlobalInfo "STR" euStr [Strict]
  , GlobalInfo "SYM" euSym [Strict]
  , GlobalInfo "LETTERS" euLetters [Strict]
  ]



euMatch :: LambdaForm
euMatch =
  lam_ 0 2 $
  ann_ "__MATCH" 0 $
  force_ (Atom (Local 0)) $
  force_ (Atom (Local 1)) $
  appbif_ (intrinsicIndex "MATCH") [Local 2, Local 3]



euMatches :: LambdaForm
euMatches =
  lam_ 0 2 $
  ann_ "__MATCHES" 0 $
  force_ (Atom (Local 0)) $
  force_ (Atom (Local 1)) $
  appbif_ (intrinsicIndex "MATCHES") [Local 2, Local 3]



euJoin :: LambdaForm
euJoin =
  lam_ 0 2 $
  ann_ "__JOIN" 0 $
  let_
    [pc_ [Local 0] $ thunkn_ 1 $ appfn_ (Global "seqNatList") [Local 0]]
    (force_
       (Atom (Local 1))
       (force_ (Atom (Local 2)) $
        appbif_ (intrinsicIndex "JOIN") [Local 4, Local 3]))


-- | SPLIT(s, re)
euSplit :: LambdaForm
euSplit =
  let s = Local 0
      re = Local 1
      es = Local 2
      ere = Local 3
   in lam_ 0 2 $
      ann_ "__SPLIT" 0 $
      force_
        (Atom s)
        (force_ (Atom re) $ appbif_ (intrinsicIndex "SPLIT") [es, ere])


-- | __STR(n) - only supports natives for now
euStr :: LambdaForm
euStr =
  lam_ 0 1 $
  ann_ "__STR" 0 $
  force_ (Atom (Local 0)) (appbif_ (intrinsicIndex "STRNAT") [Local 1])


-- | __SYM(n)
euSym :: LambdaForm
euSym =
  lam_ 0 1 $
  ann_ "__SYM" 0 $
  force_ (Atom (Local 0)) (appbif_ (intrinsicIndex "STRSYM") [Local 1])


-- | __LETTERS(s)
euLetters :: LambdaForm
euLetters =
  lam_ 0 1 $
  ann_ "__LETTERS" 0 $
  force_ (Atom (Local 0)) (appbif_ (intrinsicIndex "LETTERS") [Local 1])
