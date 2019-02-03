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
module Eucalypt.Stg.Syn where

import Data.Foldable (toList)
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import Data.Word
import Eucalypt.Core.SourceMap (SMID)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Pretty
import qualified Text.PrettyPrint as P


-- | The various types of reference from STG code to other closures
-- and values.
data Ref
  = Global !String
  | Local !Word64
  | Literal !Native
  deriving (Eq, Show)

envIndex :: Ref -> Maybe Word64
envIndex (Local n) = Just n
envIndex _ = Nothing

-- | Vector of Ref, describing source of free variables
type RefVec = Seq.Seq Ref

-- | Create a ref vec of local environments references from 'from' to
-- 'to'.
locals :: Word64 -> Word64 -> RefVec
locals from to =
  Local . fromIntegral <$> Seq.iterateN (fromIntegral (to - from)) (+ 1) from

localsList :: Int -> Int -> [Ref]
localsList from to = [Local $ fromIntegral i | i <- [from .. to - 1]]

-- | Extract refs from a syntax expression
class Show a =>
      HasRefs a
  where
  refs :: a -> [Ref]

instance StgPretty Ref where
  prettify (Global s) = P.char 'G' <> P.brackets (P.text s)
  prettify (Local i) = P.char 'E' <> P.brackets (P.int (fromIntegral i))
  prettify (Literal n) = prettify n

-- | References to stack or env are contextual, global vars not.
isContextualRef :: Ref -> Bool
isContextualRef (Global _) = False
isContextualRef _ = True

-- | Constructor tag
type Tag = Word64

-- | Branches of a case expression.
--
-- Matching data structures via constructor and forcing native scalars
-- are both handled withing the same case statement, enabling deep
-- tree walking of a data structure down to and including the leaf
-- scalars without having to worry about boxing the the natives.
data BranchTable = BranchTable
  { dataBranches :: Map.Map Tag (Word64, StgSyn)
    -- ^ Map constructor tags, to expression and number of variables
    -- to bind (which might be the arity of the constructor or arity
    -- of constructor + 1 to receive metadata)
  , defaultBranch :: Maybe StgSyn
    -- ^ Default branch only receives one binding because in each case
    -- (constructor, native, ...), the metadata is persisted through
    -- the environment
  } deriving (Eq, Show)

instance HasRefs BranchTable where
  refs (BranchTable brs df) =
    foldMap (refs . snd) brs <> foldMap refs df

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

instance HasRefs Func where
  refs (Ref r) = [r]
  refs _ = []

instance StgPretty Func where
  prettify (Ref r) = prettify r
  prettify (Con t) = P.text "C|" <> P.int (fromIntegral t)
  prettify (Intrinsic i) = P.text "__" <> P.brackets (P.int i)

-- | A lambda form - the primary component of heap stored objects,
-- including both free and bound variable book-keeping and an
-- updateable flag.
data LambdaForm = LambdaForm
  { _free :: !Word64
  , _bound :: !Word64
  , _update :: !Bool
  , _body :: !StgSyn
  } deriving (Eq, Show)

instance HasRefs LambdaForm where
  refs lf = refs $ _body lf

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
  { pcFreeRefs :: !RefVec
    -- ^ Refs into existing environment to use to form local
    -- environment for the closure
  , pcMetaRef :: !(Maybe Ref)
    -- ^ Optional ref into existing environment representing metadata
    -- tagged onto the resulting value
  , pcLambdaForm :: !LambdaForm
    -- ^ 'LambdaForm' containing expression to evaluate
  } deriving (Eq, Show)

instance HasRefs PreClosure where
  refs (PreClosure rv m _) = toList rv <> toList m

instance StgPretty PreClosure where
  prettify (PreClosure rs md lf) = refDoc <> metaDoc <> P.space <> prettify lf
    where
      refDoc =
        P.braces $
        P.hcat $ P.punctuate (P.comma <> P.space) (map prettify (toList rs))
      metaDoc = case md of
        Just r -> P.char '`' <> prettify r <> P.char '`'
        Nothing -> P.empty

-- | The STG language
data StgSyn
  = Atom !Ref
  | Case !StgSyn
         !BranchTable
  | App !Func
        !RefVec
  | Let (Seq.Seq PreClosure)
        !StgSyn
  | LetRec (Seq.Seq PreClosure)
           !StgSyn
  | Ann !String !SMID !StgSyn
  deriving (Eq, Show)

instance HasRefs StgSyn where
  refs (Atom r) = [r]
  refs (Case r k) = refs r <> refs k
  refs (App f xs) = refs f <> toList xs
  refs (Let pcs expr) = foldMap refs pcs <> refs expr
  refs (LetRec pcs expr) = foldMap refs pcs <> refs expr
  refs (Ann _ _ expr) = refs expr

instance StgPretty StgSyn where
  prettify (Atom r) = P.char '*' <> prettify r
  prettify (Case s k) =
    (P.text "case" <> P.space <> prettify s <> P.space <> P.text "of") P.$$
    P.nest 2 (prettify k)
  prettify (App f xs) =
    prettify f <> P.space <> P.hcat (P.punctuate P.space (map prettify (toList xs)))
  prettify (Let pcs e) =
    P.hang (P.text "let") 4 (P.vcat (map prettify (toList pcs))) P.$$
    P.nest 1 ( P.text "in" <> P.space <> prettify e)
  prettify (LetRec pcs e) =
    P.hang (P.text "letrec") 7 (P.vcat (map prettify (toList pcs))) P.$$
    P.nest 1 ( P.text "in" <> P.space <> prettify e)
  prettify (Ann s _ expr) = P.char '`' <> P.text s <> P.char '`' <> P.space <> prettify expr

force_ :: StgSyn -> StgSyn -> StgSyn
force_ scrutinee df = Case scrutinee (BranchTable mempty (Just df))

case_ :: StgSyn -> [(Tag, (Word64, StgSyn))] -> StgSyn
case_ scrutinee cases =
  Case scrutinee (BranchTable (Map.fromList cases) Nothing)

casedef_ :: StgSyn -> [(Tag, (Word64, StgSyn))] -> StgSyn -> StgSyn
casedef_ scrutinee cases df =
  Case scrutinee (BranchTable (Map.fromList cases) $ Just df)

let_ :: [PreClosure] -> StgSyn -> StgSyn
let_ pcs = Let (Seq.fromList pcs)

letrec_ :: [PreClosure] -> StgSyn -> StgSyn
letrec_ pcs = LetRec (Seq.fromList pcs)

appfn_ :: Ref -> [Ref] -> StgSyn
appfn_ f xs = App (Ref f) $ Seq.fromList xs

appbif_ :: Int -> [Ref] -> StgSyn
appbif_ f xs = App (Intrinsic f) $ Seq.fromList xs

appcon_ :: Tag -> [Ref] -> StgSyn
appcon_ t xs = App (Con t) $ Seq.fromList xs

lam_ :: Int -> Int -> StgSyn -> LambdaForm
lam_ f b = LambdaForm (fromIntegral f) (fromIntegral b) False

thunkn_ :: Int -> StgSyn -> LambdaForm
thunkn_ n = LambdaForm (fromIntegral n) 0 True

thunk_ :: StgSyn -> LambdaForm
thunk_ = thunkn_ 0

box_ :: Native -> LambdaForm
box_ n = LambdaForm 0 0 False (Atom (Literal n))

valuen_ :: Int -> StgSyn -> LambdaForm
valuen_ n = LambdaForm (fromIntegral n) 0 False

value_ :: StgSyn -> LambdaForm
value_ = valuen_ 0

seq_ :: StgSyn -> StgSyn -> StgSyn
seq_ a b = Case a $ BranchTable mempty (Just b)

seqall_ :: [StgSyn] -> StgSyn
seqall_ = foldl1 seq_

pc0_ :: LambdaForm -> PreClosure
pc0_ = PreClosure mempty Nothing

pc0m_ :: Ref -> LambdaForm -> PreClosure
pc0m_ meta = PreClosure mempty (Just meta)

pc_ :: [Ref] -> LambdaForm -> PreClosure
pc_ = (`PreClosure` Nothing) . Seq.fromList

pcm_ :: [Ref] -> Maybe Ref -> LambdaForm -> PreClosure
pcm_ rs = PreClosure (Seq.fromList rs)

ann_ :: String -> SMID -> StgSyn -> StgSyn
ann_ = Ann

-- | Standard constructor - applies saturated data constructor of tag
-- @t@ to refs on stack.
standardConstructor :: Word64 -> Tag -> LambdaForm
standardConstructor f t =
  LambdaForm f 0 False $
  App (Con t) $ Local <$> Seq.iterateN (fromIntegral f) (+ 1) 0
