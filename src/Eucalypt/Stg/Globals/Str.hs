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

import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)

globals :: [(String, LambdaForm)]
globals =
  [ ("MATCHES", euMatches)
  , ("MATCH", euMatch)
  , ("JOIN", euJoin)
  , ("SPLIT", euSplit)
  , ("STR", euStr)
  , ("SYM", euSym)
  , ("LETTERS", euLetters)
  , ("DQ", euDoubleQuote)
  , ("FMT", euFormat)
  , ("UPPER", euUpper)
  , ("LOWER", euLower)
  ]


euMatch :: LambdaForm
euMatch =
  lam_ 0 2 $
  ann_ "__MATCH" 0 $
  force_ (Atom (L 0)) $
  force_ (Atom (L 1)) $
  appbif_ (intrinsicIndex "MATCH") [L 2, L 3]



euMatches :: LambdaForm
euMatches =
  lam_ 0 2 $
  ann_ "__MATCHES" 0 $
  force_ (Atom (L 0)) $
  force_ (Atom (L 1)) $
  appbif_ (intrinsicIndex "MATCHES") [L 2, L 3]



euJoin :: LambdaForm
euJoin =
  lam_ 0 2 $
  ann_ "__JOIN" 0 $
  let_
    [pc_ [L 0] $ thunkn_ 1 $ appfn_ (gref "seqNatList") [L 0]]
    (force_
       (Atom (L 1))
       (force_ (Atom (L 2)) $
        appbif_ (intrinsicIndex "JOIN") [L 4, L 3]))


-- | SPLIT(s, re)
euSplit :: LambdaForm
euSplit =
  let s = L 0
      re = L 1
      es = L 2
      ere = L 3
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
  force_ (Atom (L 0)) (appbif_ (intrinsicIndex "STRNAT") [L 1])


-- | __SYM(n)
euSym :: LambdaForm
euSym =
  lam_ 0 1 $
  ann_ "__SYM" 0 $
  force_ (Atom (L 0)) (appbif_ (intrinsicIndex "STRSYM") [L 1])


-- | __LETTERS(s)
euLetters :: LambdaForm
euLetters =
  lam_ 0 1 $
  ann_ "__LETTERS" 0 $
  force_ (Atom (L 0)) (appbif_ (intrinsicIndex "LETTERS") [L 1])


-- | DQ Constant for double quote (while we don't support standard string escapes )
euDoubleQuote :: LambdaForm
euDoubleQuote = value_ (Atom $ V $ NativeString "\"")


-- | __FMT(obj, spec) format a native object according to a format identifier
euFormat :: LambdaForm
euFormat =
  lam_ 0 2 $
  ann_ "__FMT" 0 $
  force_ (Atom (L 0)) $
  force_ (Atom (L 1)) $ appbif_ (intrinsicIndex "FMT") [L 2, L 3]


-- | __UPPER(s) - upper case a string
euUpper :: LambdaForm
euUpper =
  lam_ 0 1 $
  ann_ "__UPPER" 0 $
  force_ (Atom (L 0)) $ appbif_ (intrinsicIndex "UPPER") [L 1]



-- | __LOWER(s) - lower case a string
euLower :: LambdaForm
euLower =
  lam_ 0 1 $
  ann_ "__LOWER" 0 $
  force_ (Atom (L 0)) $ appbif_ (intrinsicIndex "LOWER") [L 1]
