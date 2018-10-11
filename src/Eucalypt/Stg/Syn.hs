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

import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.List (maximum)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (All(..))
import Data.Scientific
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary(..), Gen, oneof)
import qualified Text.PrettyPrint as P
import Eucalypt.Core.SourceMap (SMID)

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
  prettify (NativeString s) = P.text $ show s
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
  | Literal !Native
  deriving (Eq, Show)

envIndex :: Ref -> Maybe Word64
envIndex (Local n) = Just n
envIndex _ = Nothing

-- | Vector of Ref, describing source of free variables
type RefVec = Vector Ref

-- | Create a ref vec of local environments references from 'from' to
-- 'to'.
locals :: Word64 -> Word64 -> RefVec
locals from to =
  V.generate (fromIntegral (to - from)) $ Local . (from +) . fromIntegral

localsList :: Int -> Int -> [Ref]
localsList from to = [Local $ fromIntegral i | i <- [from .. to - 1]]

-- | Extract refs from a syntax expression
class Show a =>
      HasRefs a
  where
  refs :: a -> [Ref]
  validateRefs :: Int -> a -> Bool
  validateRefs size expr =
    case mapMaybe (fmap fromIntegral . envIndex) $ refs expr of
      [] -> True
      is ->
        let m = maximum is
         in (m <= size) ||
            error
              ("Local " ++
               show m ++
               " exceeds env length " ++ show size ++ " in " ++ show expr)


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
-- Matching data structures via constructor and native scalars via
-- equality are both handled within the same case statement, enabling
-- deep tree walking of a data structure down to and including the
-- leaf scalars without having to worry about boxing the the natives
-- to signal a need for a caselit expression instead of a case.
--
-- (The scalars are after all "boxed" with the 'Native' type, there is
-- not much sense boxing them again in STG.)
data BranchTable = BranchTable
  { dataBranches :: Map.Map Tag (Word64, StgSyn)
  , nativeBranches :: HM.HashMap Native StgSyn
  , defaultBranch :: Maybe StgSyn
  } deriving (Eq, Show)

instance HasRefs BranchTable where
  refs (BranchTable brs nbrs df) =
    foldMap (refs . snd) brs <> foldMap refs nbrs <> foldMap refs df
  validateRefs size (BranchTable brs nbrs df) =
    getAll $
    foldMap (\(n, e) -> All $ validateRefs (size + fromIntegral n) e) brs <>
    foldMap (All . validateRefs (size + 1)) nbrs <>
    foldMap (All . validateRefs (size + 1)) df

instance StgPretty BranchTable where
  prettify (BranchTable bs nbs df) =
    case df of
      Just d ->
        branchesDoc P.$$ nativeBranchesDoc P.$$ (P.text "_ -> " <> prettify d)
      Nothing -> branchesDoc
    where
      branchesDoc = Map.foldrWithKey accBr P.empty bs
      accBr k (binds, syn) doc =
        doc P.$$ P.text "C|" <> P.int (fromIntegral k) <> P.space <> P.text "->" <>
        P.space <>
        P.parens (P.int $ fromIntegral binds) <>
        P.space <>
        prettify syn
      nativeBranchesDoc = HM.foldrWithKey accNBr P.empty nbs
      accNBr n syn doc =
        doc P.$$
        (prettify n <> P.space <> P.text "->" <> P.space <> prettify syn)

data Func
  = Ref !Ref -- closure ref
  | Con !Tag
  | Intrinsic !Int
  deriving (Eq, Show)

instance HasRefs Func where
  refs (Ref r) = [r]
  refs _ = []
  validateRefs size (Ref (Local r)) = fromIntegral r < size
  validateRefs _ _ = True

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
  validateRefs size lf =
    validateRefs (size + fromIntegral (_bound lf)) $ _body lf

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
  | Let (Vector PreClosure)
        !StgSyn
  | LetRec (Vector PreClosure)
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
  validateRefs size (Case r k) = validateRefs size r && validateRefs size k
  validateRefs size (Let pcs expr) =
    getAll (foldMap (All . validateRefs size) pcs) &&
    validateRefs (size + length pcs) expr
  validateRefs size (LetRec pcs expr) =
    getAll (foldMap (All . validateRefs (size + length pcs)) pcs) &&
    validateRefs (size + length pcs) expr
  validateRefs size expr =
    case mapMaybe (fmap fromIntegral . envIndex) $ refs expr of
      [] -> True
      is -> size > maximum is

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
force_ scrutinee df = Case scrutinee (BranchTable mempty mempty (Just df))

case_ :: StgSyn -> [(Tag, (Word64, StgSyn))] -> StgSyn
case_ scrutinee cases =
  Case scrutinee (BranchTable (Map.fromList cases) mempty Nothing)

casedef_ :: StgSyn -> [(Tag, (Word64, StgSyn))] -> StgSyn -> StgSyn
casedef_ scrutinee cases df =
  Case scrutinee (BranchTable (Map.fromList cases) mempty $ Just df)

caselit_ :: StgSyn -> [(Native, StgSyn)] -> Maybe StgSyn -> StgSyn
caselit_ scrutinee cases df =
  Case scrutinee (BranchTable mempty (HM.fromList cases) df)

polycase_ ::
     StgSyn
  -> [(Tag, (Word64, StgSyn))]
  -> [(Native, StgSyn)]
  -> Maybe StgSyn
  -> StgSyn
polycase_ scrutinee bs nbs df =
  Case scrutinee (BranchTable (Map.fromList bs) (HM.fromList nbs) df)

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

valuen_ :: Int -> StgSyn -> LambdaForm
valuen_ n = LambdaForm (fromIntegral n) 0 False

value_ :: StgSyn -> LambdaForm
value_ = valuen_ 0

seq_ :: StgSyn -> StgSyn -> StgSyn
seq_ a b = Case a $ BranchTable mempty mempty (Just b)

seqall_ :: [StgSyn] -> StgSyn
seqall_ = foldl1 seq_

pc0_ :: LambdaForm -> PreClosure
pc0_ = PreClosure mempty Nothing

pc0m_ :: Ref -> LambdaForm -> PreClosure
pc0m_ meta = PreClosure mempty (Just meta)

pc_ :: [Ref] -> LambdaForm -> PreClosure
pc_ = (`PreClosure` Nothing) . V.fromList

pcm_ :: [Ref] -> Maybe Ref -> LambdaForm -> PreClosure
pcm_ rs = PreClosure (V.fromList rs)

ann_ :: String -> SMID -> StgSyn -> StgSyn
ann_ = Ann

-- | Standard constructor - applies saturated data constructor of tag
-- @t@ to refs on stack.
standardConstructor :: Word64 -> Tag -> LambdaForm
standardConstructor f t =
  LambdaForm f 0 False $
  App (Con t) $ V.generate (fromIntegral f) $ Local . fromIntegral
