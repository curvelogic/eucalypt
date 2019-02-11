{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Eucalypt.Stg.Syn
Description : Syntax for spineless tagless G-machine execution
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

Originally based on
https://github.com/ermine-language/ermine/blob/master/src/Ermine/Syntax/G.hs
(-- Copyright :  (c) Edward Kmett and Dan Doel 2014)
though now not much similarity remains.
-}
module Eucalypt.Stg.Syn
  ( module Eucalypt.Stg.Syn
  , module Eucalypt.Stg.Vec
  ) where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Eucalypt.Core.SourceMap (SMID)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Pretty
import Eucalypt.Stg.Vec
import qualified Text.PrettyPrint as P

-- | Refs in the syntax may include literal natives. (In the machine
-- they can contain general values including addresses)
type Ref = Reference Native

-- | Vector of Ref, describing source of free variables
type SynVec = RefVec Native

-- | Constructor tag
type Tag = Word

-- | Branches of a case expression.
--
-- Matching data structures via constructor and forcing native scalars
-- are both handled withing the same case statement, enabling deep
-- tree walking of a data structure down to and including the leaf
-- scalars without having to worry about boxing the the natives.
data BranchTable = BranchTable
  { dataBranches :: Map.Map Tag (Int, StgSyn)
    -- ^ Map constructor tags, to expression and number of variables
    -- to bind (which might be the arity of the constructor or arity
    -- of constructor + 1 to receive metadata)
  , defaultBranch :: Maybe StgSyn
    -- ^ Default branch only receives one binding because in each case
    -- (constructor, native, ...), the metadata is persisted through
    -- the environment
  } deriving (Eq, Show)

instance StgPretty BranchTable where
  prettify (BranchTable bs df) =
    case df of
      Just d ->
        branchesDoc P.$$ (P.text "_ -> " <> prettify d)
      Nothing -> branchesDoc
    where
      branchesDoc = Map.foldrWithKey accBr P.empty bs
      accBr k (binds, syn) doc =
        doc P.$$ P.text "C|" <> P.int (fromIntegral k) <> P.space <> P.text "->" <>
        P.space <>
        P.parens (P.int $ fromIntegral binds) <>
        P.space <>
        prettify syn


-- | The type of callable
data Func
  = Ref !Ref -- ^ Ref to a closure
  | Con !Tag -- ^ Constructor tag
  | Intrinsic !Int -- ^ An intrinsic function
  deriving (Eq, Show)

instance StgPretty Func where
  prettify (Ref r) = prettify r
  prettify (Con t) = P.text "C|" <> P.int (fromIntegral t)
  prettify (Intrinsic i) = P.text "__" <> P.brackets (P.int i)

-- | A lambda form - the primary component of heap stored objects,
-- including both free and bound variable book-keeping and an
-- updateable flag.
data LambdaForm = LambdaForm
  { lamFree :: !Int
    -- ^ count of free variables (refs into environment) in the lambda
    -- form.
  , lamBound :: !Int
    -- ^ count of bound variables (i.e. arity) of the lambda form
  , lamUpdate :: !Bool
    -- ^ whether to update (i.e. is this a thunk)
  , lamBody :: !StgSyn
    -- ^ lambda body
  } deriving (Eq, Show)

instance StgPretty LambdaForm where
  prettify (LambdaForm f b u body) =
    P.text "\\" <> P.space <> P.int (fromIntegral f) <> P.space <>
    P.int (fromIntegral b) <>
    P.space <>
    P.text
      (if u
         then "=>"
         else "->") <>
    P.space <>
    prettify body

-- | A PreClosure is a LambdaForm together with refs which will be
-- resolved to the bindings at allocation time.
data PreClosure = PreClosure
  { pcFreeRefs :: !SynVec
    -- ^ Refs into existing environment to use to form local
    -- environment for the closure
  , pcMetaRef :: !(Maybe Ref)
    -- ^ Optional ref into existing environment representing metadata
    -- tagged onto the resulting value
  , pcLambdaForm :: !LambdaForm
    -- ^ 'LambdaForm' containing expression to evaluate
  } deriving (Eq, Show)

instance StgPretty PreClosure where
  prettify (PreClosure rs md lf) =
    P.braces (prettify rs) <> metaDoc <> P.space <> prettify lf
    where
      metaDoc =
        case md of
          Just r -> P.char '`' <> prettify r <> P.char '`'
          Nothing -> P.empty

-- | The STG language
data StgSyn
  = Atom { synEvaluand :: !Ref }
  | Case { synScrutinee :: !StgSyn
         , synBranches :: !BranchTable }
  | App { synCallable :: !Func
        , synArguments :: !SynVec }
  | Let { synBindings :: Seq.Seq PreClosure
        , synBody :: !StgSyn }
  | LetRec { synBindings :: Seq.Seq PreClosure
           , synBody :: !StgSyn }
  | Ann { synAnnotation :: !String
        , synSmid :: !SMID
        , synBody :: !StgSyn }
  deriving (Eq, Show)

instance StgPretty StgSyn where
  prettify (Atom r) = P.char '*' <> prettify r
  prettify (Case s k) =
    (P.text "case" <> P.space <> prettify s <> P.space <> P.text "of") P.$$
    P.nest 2 (prettify k)
  prettify (App f xs) =
    prettify f <> P.space <> prettify xs
  prettify (Let pcs e) =
    P.hang (P.text "let") 4 (P.vcat (map prettify (toList pcs))) P.$$
    P.nest 1 ( P.text "in" <> P.space <> prettify e)
  prettify (LetRec pcs e) =
    P.hang (P.text "letrec") 7 (P.vcat (map prettify (toList pcs))) P.$$
    P.nest 1 ( P.text "in" <> P.space <> prettify e)
  prettify (Ann s _ expr) = P.char '`' <> P.text s <> P.char '`' <> P.space <> prettify expr

atom_ :: Ref -> StgSyn
atom_ = Atom

force_ :: StgSyn -> StgSyn -> StgSyn
force_ scrutinee df = Case scrutinee (BranchTable mempty (Just df))

forceall_ :: [StgSyn] -> StgSyn
forceall_ = foldl1 force_

case_ :: StgSyn -> [(Tag, (Int, StgSyn))] -> StgSyn
case_ scrutinee cases =
  Case scrutinee (BranchTable (Map.fromList cases) Nothing)

casedef_ :: StgSyn -> [(Tag, (Int, StgSyn))] -> StgSyn -> StgSyn
casedef_ scrutinee cases df =
  Case scrutinee (BranchTable (Map.fromList cases) $ Just df)

let_ :: [PreClosure] -> StgSyn -> StgSyn
let_ pcs = Let (Seq.fromList pcs)

letrec_ :: [PreClosure] -> StgSyn -> StgSyn
letrec_ pcs = LetRec (Seq.fromList pcs)

appfn_ :: Ref -> [Ref] -> StgSyn
appfn_ f xs = App (Ref f) $ refs xs

appbif_ :: Int -> [Ref] -> StgSyn
appbif_ f xs = App (Intrinsic f) $ refs xs

appcon_ :: Tag -> [Ref] -> StgSyn
appcon_ t xs = App (Con t) $ refs xs

lam_ :: Int -> Int -> StgSyn -> LambdaForm
lam_ f b = LambdaForm (fromIntegral f) (fromIntegral b) False

thunkn_ :: Int -> StgSyn -> LambdaForm
thunkn_ n = LambdaForm (fromIntegral n) 0 True

thunk_ :: StgSyn -> LambdaForm
thunk_ = thunkn_ 0

box_ :: Native -> LambdaForm
box_ n = LambdaForm 0 0 False (Atom (V n))

valuen_ :: Int -> StgSyn -> LambdaForm
valuen_ n = LambdaForm (fromIntegral n) 0 False

value_ :: StgSyn -> LambdaForm
value_ = valuen_ 0

pc0_ :: LambdaForm -> PreClosure
pc0_ = PreClosure mempty Nothing

pc0m_ :: Ref -> LambdaForm -> PreClosure
pc0m_ meta = PreClosure mempty (Just meta)

pc_ :: [Ref] -> LambdaForm -> PreClosure
pc_ = (`PreClosure` Nothing) . refs

pcm_ :: [Ref] -> Maybe Ref -> LambdaForm -> PreClosure
pcm_ rs = PreClosure (refs rs)

ann_ :: String -> SMID -> StgSyn -> StgSyn
ann_ = Ann

-- | Standard constructor - applies saturated data constructor of tag
-- @t@ to refs on stack.
standardConstructor :: Int -> Tag -> LambdaForm
standardConstructor f t = LambdaForm f 0 False $ App (Con t) $ range 0 f
