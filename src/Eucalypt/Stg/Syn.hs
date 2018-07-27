{-# LANGUAGE DeriveGeneric #-}
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
import Data.Hashable
import qualified Data.Map as Map
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word
import GHC.Generics (Generic)
import qualified Text.PrettyPrint as P

class StgPretty a where
  prettify :: a -> P.Doc

-- | The various types of reference from STG code to other closures
-- and values.
data Ref
  = Global !String
  | LocalEnv !Word64
  | StackArg !Word64
  | Literal !Native
  deriving (Eq, Show)

instance StgPretty Ref where
  prettify (Global s) = P.char 'G' <> P.brackets (P.text s)
  prettify (LocalEnv i) = P.char 'E' <> P.brackets (P.int (fromIntegral i))
  prettify (StackArg i) = P.char 'S' <> P.brackets (P.int (fromIntegral i))
  prettify (Literal n) = prettify n

-- | References to stack or env are contextual, global vars not.
isContextualRef :: Ref -> Bool
isContextualRef (Global _) = False
isContextualRef _ = True

type Tag = Word64

data Continuation =
  Continuation (Map.Map Tag (Word64, StgSyn))
               (Maybe StgSyn)
  deriving (Eq, Show)

instance StgPretty Continuation where
  prettify (Continuation bs df) =
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

data NativeContinuation =
  NativeContinuation (HM.HashMap Native StgSyn)
                     (Maybe StgSyn)
  deriving (Eq, Show)

instance StgPretty NativeContinuation where
  prettify (NativeContinuation bs df) =
    case df of
      Just d -> branchesDoc P.$$ (P.text "_ -> " <> prettify d)
      Nothing -> branchesDoc
    where
      branchesDoc = HM.foldrWithKey f (P.text "") bs
      f n syn doc =
        doc P.$$
        (prettify n <> P.space <> P.text "->" <> P.space <> prettify syn)

data Func
  = Ref !Ref -- closure ref
  | Con !Tag
  | Intrinsic !Int
  deriving (Eq, Show)

instance StgPretty Func where
  prettify (Ref r) = prettify r
  prettify (Con t) = P.text "C|" <> P.int (fromIntegral t)
  prettify (Intrinsic i) = P.text "__" <> P.brackets (P.int i)

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
  PreClosure !(Vector Ref)
             !LambdaForm
  deriving (Eq, Show)

instance StgPretty PreClosure where
  prettify (PreClosure refs lf) = refDoc <> P.space <> prettify lf
    where
      refDoc =
        P.braces $
        P.hcat $ P.punctuate (P.comma <> P.space) (map prettify (toList refs))

-- | Primitives that can live on the stack.
--
-- Not worried about efficiency of representation for now.
data Native
  = NativeInt !Integer
  | NativeString !String
  | NativeBool !Bool
  deriving (Eq, Show, Generic)

instance Hashable Native

instance StgPretty Native where
  prettify (NativeInt i) = P.int (fromIntegral i)
  prettify (NativeString s) = P.text s
  prettify (NativeBool b) =
    if b
      then P.text "#t"
      else P.text "#f"

data StgSyn
  = Case !StgSyn
         !Continuation
  | CaseLit !StgSyn
            !NativeContinuation
  | App !Func
        !(Vector Ref)
  | Let (Vector PreClosure)
        !StgSyn
  | LetRec (Vector PreClosure)
           !StgSyn
  | Lit !Native
  deriving (Eq, Show)

instance StgPretty StgSyn where
  prettify (Case s k) =
    (P.text "case" <> P.space <> prettify s <> P.space <> P.text "of") P.$$
    prettify k
  prettify (CaseLit s k) =
    (P.text "case" <> P.space <> prettify s <> P.space <> P.text "of") P.$$
    prettify k
  prettify (App f xs) =
    prettify f <> P.space <> P.hcat (P.punctuate P.space (map prettify (toList xs)))
  prettify (Let pcs e) =
    P.hang (P.text "let") 4 (P.vcat (map prettify (toList pcs))) P.$$
    P.nest 1 ( P.text "in" <> P.space <> prettify e)
  prettify (LetRec pcs e) =
    P.hang (P.text "letrec") 7 (P.vcat (map prettify (toList pcs))) P.$$
    P.nest 1 ( P.text "in" <> P.space <> prettify e)
  prettify (Lit n) = prettify n

-- | Create a non-updatable lambda form
noUpdate :: Word64 -> Word64 -> StgSyn -> LambdaForm
noUpdate f b = LambdaForm f b False

-- | Create an updatable (i.e. thunk) lambda form
doUpdate :: Word64 -> StgSyn -> LambdaForm
doUpdate f = LambdaForm f 0 True

-- | Standard constructor - applies saturated data constructor of tag
-- @t@ to refs on stack.
standardConstructor :: Word64 -> Tag -> LambdaForm
standardConstructor f t =
  LambdaForm f 0 False $
  App (Con t) $ V.generate (fromIntegral f) $ LocalEnv . fromIntegral
