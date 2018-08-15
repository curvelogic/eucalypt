{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric  #-}
{-|
Module      : Eucalypt.Stg.Syn
Description : Syntax for spineless tagless G-machine execution
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

Heavily based on
https://github.com/ermine-language/ermine/blob/master/src/Ermine/Syntax/G.hs
(-- Copyright :  (c) Edward Kmett and Dan Doel 2014)
-}
module Eucalypt.Stg.Syn where

import Data.Bifunctor (second)
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.Map as Map
import Data.Scientific
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)
import qualified Text.PrettyPrint as P
import Test.QuickCheck (Arbitrary(..), oneof, Gen)

-- | Pretty printable syntax element
class StgPretty a where
  -- | Construct pretty print 'Doc' for a syntax element
  prettify :: a -> P.Doc

-- | Primitives that can live on the stack.
--
-- Not worried about efficiency of representation for now.
data Native
  = NativeNumber !Scientific
  | NativeString !String
  | NativeSymbol !String
  | NativeBool !Bool
  deriving (Eq, Show, Generic)

-- | NativeBranchTable matches natives by hash map
instance Hashable Native

instance StgPretty Native where
  prettify (NativeNumber i) = either P.float P.int $ floatingOrInteger i
  prettify (NativeString s) = P.text s
  prettify (NativeSymbol s) = P.colon <> P.text s
  prettify (NativeBool b) =
    if b
      then P.text "#t"
      else P.text "#f"

instance Arbitrary Scientific where
  arbitrary =
    oneof
      [fromInteger <$> arbitrary, fromFloatDigits <$> (arbitrary :: Gen Float)]

instance Arbitrary Native where
  arbitrary =
    oneof
      [ NativeNumber <$> arbitrary
      , NativeString <$> arbitrary
      , NativeSymbol <$> arbitrary
      , NativeBool <$> arbitrary
      ]

-- | The various types of reference from STG code to other closures
-- and values.
data Ref
  = Global !String
  | Local !Word64
  | BoundArg !Word64
  | Literal !Native
  deriving (Eq, Show)

-- | Vector of Ref, describing source of free variables
type RefVec = Vector Ref

-- | Create a ref vec of local environments references from 'from' to
-- 'to'.
locals :: Word64 -> Word64 -> RefVec
locals from to =
  V.generate (fromIntegral (to - from)) $ Local . (from +) . fromIntegral

localsList :: Int -> Int -> [Ref]
localsList from to = [Local $ fromIntegral i | i <- [from .. to - 1]]

-- | Something which has reference to (bound arguments); once these
-- are located in an environment they can become environment refs
-- instead
class HasArgRefs a where
  argsAt :: Int -> a -> a

-- | Once args are in environment vector, we can replace the refs
instance HasArgRefs Ref where
  argsAt n (BoundArg i) = Local (fromIntegral n + i)
  argsAt _ r = r

instance StgPretty Ref where
  prettify (Global s) = P.char 'G' <> P.brackets (P.text s)
  prettify (Local i) = P.char 'E' <> P.brackets (P.int (fromIntegral i))
  prettify (BoundArg i) = P.char 'S' <> P.brackets (P.int (fromIntegral i))
  prettify (Literal n) = prettify n

-- | References to stack or env are contextual, global vars not.
isContextualRef :: Ref -> Bool
isContextualRef (Global _) = False
isContextualRef _ = True

-- | Constructor tag
type Tag = Word64

class HasDefaultBranch a where
  defaultBranch :: a -> Maybe StgSyn

-- | Branches of a case expression matching constructors and binding
-- arguments
data BranchTable =
  BranchTable (Map.Map Tag (Word64, StgSyn))
               (Maybe StgSyn)
  deriving (Eq, Show)

instance HasDefaultBranch BranchTable where
  defaultBranch (BranchTable _ df) = df

instance HasArgRefs BranchTable where
  argsAt n (BranchTable branches df) = BranchTable branches' df'
    where
      branches' = Map.map (second (argsAt n)) branches
      df' = argsAt n <$> df

instance StgPretty BranchTable where
  prettify (BranchTable bs df) =
    case df of
      Just d -> branchesDoc P.$$ (P.text "_ -> " <> prettify d)
      Nothing -> branchesDoc
    where
      branchesDoc = Map.foldrWithKey f (P.text "") bs
      f k (binds, syn) doc =
        doc P.$$ P.int (fromIntegral k) <> P.space <> P.text "->" <> P.space <>
        P.parens (P.int $ fromIntegral binds) <>
        P.space <>
        prettify syn

-- | Branches of a caselit expression matching native values
data NativeBranchTable =
  NativeBranchTable (HM.HashMap Native StgSyn)
                     (Maybe StgSyn)
  deriving (Eq, Show)

instance HasDefaultBranch NativeBranchTable where
  defaultBranch (NativeBranchTable _ df) = df

instance HasArgRefs NativeBranchTable where
  argsAt n (NativeBranchTable branches df) = NativeBranchTable branches' df'
    where
      branches' = HM.map (argsAt n) branches
      df' = argsAt n <$> df

instance StgPretty NativeBranchTable where
  prettify (NativeBranchTable bs df) =
    case df of
      Just d -> branchesDoc P.$$ (P.text "_ -> " <> prettify d)
      Nothing -> branchesDoc
    where
      branchesDoc = HM.foldrWithKey f P.empty bs
      f n syn doc =
        doc P.$$
        (prettify n <> P.space <> P.text "->" <> P.space <> prettify syn)

data Func
  = Ref !Ref -- closure ref
  | Con !Tag
  | Intrinsic !Int
  deriving (Eq, Show)

instance HasArgRefs Func where
  argsAt n (Ref r) = Ref $ argsAt n r
  argsAt _ f = f

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
data PreClosure =
  PreClosure !RefVec
             !LambdaForm
  deriving (Eq, Show)

instance HasArgRefs PreClosure where
  argsAt n (PreClosure refs lf) = PreClosure (V.map (argsAt n) refs) lf

instance StgPretty PreClosure where
  prettify (PreClosure refs lf) = refDoc <> P.space <> prettify lf
    where
      refDoc =
        P.braces $
        P.hcat $ P.punctuate (P.comma <> P.space) (map prettify (toList refs))

-- | The STG language
data StgSyn
  = Atom !Ref
  | Case !StgSyn
         !BranchTable
  | CaseLit !StgSyn
            !NativeBranchTable
  | App !Func
        !RefVec
  | Let (Vector PreClosure)
        !StgSyn
  | LetRec (Vector PreClosure)
           !StgSyn
  deriving (Eq, Show)

instance HasArgRefs StgSyn where
  argsAt n (Atom (BoundArg i)) = Atom (Local (fromIntegral n + i))
  argsAt _ a@Atom {} = a
  argsAt n (Let pcs syn) = Let (V.map (argsAt n) pcs) (argsAt n syn)
  argsAt n (LetRec pcs syn) = LetRec (V.map (argsAt n) pcs) (argsAt n syn)
  argsAt n (App f refs) = App (argsAt n f) (V.map (argsAt n) refs)
  argsAt n (Case expr k) = Case (argsAt n expr) (argsAt n k)
  argsAt n (CaseLit expr k) = CaseLit (argsAt n expr) (argsAt n k)

instance StgPretty StgSyn where
  prettify (Atom r) = P.char '*' <> prettify r
  prettify (Case s k) =
    (P.text "case" <> P.space <> prettify s <> P.space <> P.text "of") P.$$
    P.nest 2 (prettify k)
  prettify (CaseLit s k) =
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

force_ :: StgSyn -> StgSyn -> StgSyn
force_ scrutinee df = Case scrutinee (BranchTable mempty (Just df))

case_ :: StgSyn -> [(Tag, (Word64, StgSyn))] -> StgSyn
case_ scrutinee cases =
  Case scrutinee (BranchTable (Map.fromList cases) Nothing)

caselit_ :: StgSyn -> [(Native, StgSyn)] -> Maybe StgSyn -> StgSyn
caselit_ scrutinee cases df =
  CaseLit scrutinee (NativeBranchTable (HM.fromList cases) df)

forcelit_ :: StgSyn -> StgSyn -> StgSyn
forcelit_ scrutinee df = CaseLit scrutinee (NativeBranchTable mempty (Just df))

let_ :: [PreClosure] -> StgSyn -> StgSyn
let_ pcs = Let (V.fromList pcs)

letrec_ :: [PreClosure] -> StgSyn -> StgSyn
letrec_ pcs = LetRec (V.fromList pcs)

appfn_ :: Ref -> [Ref] -> StgSyn
appfn_ f xs = App (Ref f) $ V.fromList xs

appbif_ :: Int -> [Ref] -> StgSyn
appbif_ f xs = App (Intrinsic f) $ V.fromList xs

appcon_ :: Tag -> [Ref] -> StgSyn
appcon_ t xs = App (Con t) $ V.fromList xs

lam_ :: Int -> Int -> StgSyn -> LambdaForm
lam_ f b = LambdaForm (fromIntegral f) (fromIntegral b) False

thunkn_ :: Int -> StgSyn -> LambdaForm
thunkn_ n = LambdaForm (fromIntegral n) 0 True

thunk_ :: StgSyn -> LambdaForm
thunk_ = thunkn_ 0

box_ :: Native -> LambdaForm
box_ n = LambdaForm 0 0 False (Atom (Literal n))

value_ :: StgSyn -> LambdaForm
value_ = LambdaForm 0 0 False

seq_ :: StgSyn -> StgSyn -> StgSyn
seq_ a b = Case a $ BranchTable mempty (Just b)

seqall_ :: [StgSyn] -> StgSyn
seqall_ = foldl1 seq_

pc0_ :: LambdaForm -> PreClosure
pc0_ = PreClosure mempty

pc_ :: [Ref] -> LambdaForm -> PreClosure
pc_ = PreClosure . V.fromList

-- | Standard constructor - applies saturated data constructor of tag
-- @t@ to refs on stack.
standardConstructor :: Word64 -> Tag -> LambdaForm
standardConstructor f t =
  LambdaForm f 0 False $
  App (Con t) $ V.generate (fromIntegral f) $ Local . fromIntegral
