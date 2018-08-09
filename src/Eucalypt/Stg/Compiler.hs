{-|
Module      : Eucalypt.Stg.Compiler
Description : Compile core to STG-code
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

Heavily based on Ermine STG implementation
(-- Copyright :  (c) Edward Kmett and Dan Doel 2014)
-}
module Eucalypt.Stg.Compiler where

import Bound.Var as Var
import Bound.Scope (fromScope)
import qualified Data.Array as A
import Data.Foldable (toList)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Eucalypt.Core.Syn as C
import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- $ Building Blocks
--
-- Basic building blocks for compilation include constructor tags,
-- standard constructors, smart constructors and recipes.

nilConstructor :: LambdaForm
nilConstructor = standardConstructor 0 stgNil

consConstructor :: LambdaForm
consConstructor = standardConstructor 2 stgCons

blockConstructor :: LambdaForm
blockConstructor = standardConstructor 1 stgBlock

unitConstructor :: LambdaForm
unitConstructor = standardConstructor 0 stgUnit

head_ :: LambdaForm
head_ =
  LambdaForm
    0
    1
    False
    (case_
       (Atom (BoundArg 0))
       [(stgCons, (2, Atom (Local 1)))]) -- binds after
    -- stack binds

-- | Construct a list asa STG LetRec
list_ :: Int -> [Ref] -> StgSyn
list_ _ [] = appcon_ stgNil mempty
list_ envSize refs = letrec_ (pc0 : pcs) (App (Ref (Local pcn)) mempty)
  where
    pc0 = pc0_ nilConstructor
    preclose (i, r) = pc_ [r, Local $ fromIntegral i] consConstructor
    pcs = zipWith (curry preclose) [envSize ..] $ reverse refs
    pcn = fromIntegral $ envSize + length pcs

litList_ :: Int -> [Native] -> StgSyn
litList_ envSize nats = list_ envSize $ map Literal nats

-- | Compile a let / letrec binding
compileBinding :: Eq v => Int -> (v -> Ref) -> C.CoreExp v -> PreClosure
compileBinding _ context expr = pc_ free $ compileLambdaForm expr
  where
    fvs = [(v, context v) | v <- nub . toList $ expr]
    free = map snd fvs
    context' v = fromMaybe (context v) (lookup v fvs)
    contextL' (Var.F v) = context' v
    contextL' (Var.B i) = BoundArg $ fromIntegral i
    compileLambdaForm e =
      case e of
        (CoreLambda ns body) ->
          lam_ (length free) (length ns) $
          compile (length free) contextL' $ fromScope body
        (CoreList []) -> nilConstructor -- TODO: id all std cons?
        CorePrim{} -> value_ $ compile (length free) context' e
        _ -> thunkn_ (length free) $ compile (length free) context' e


-- | Compile a CoreExp into STG expression
compile :: Eq v => Int -> (v -> Ref) -> C.CoreExp v -> StgSyn

-- | Compile a let. All Core lets are potentially recursive
compile envSize context (C.CoreLet bs b) = letrec_ stgBindings stgBody
  where
    l = length bs
    envSize' = envSize + l
    stgBindings = map (compileBinding envSize' context' . fromScope . snd) bs
    stgBody = compile envSize' context' $ fromScope b
    context' = extendContextForScope envSize context l

-- | Compile a var
compile _ context (C.CoreVar v) = Atom $ context v

-- | Compile a builtin on its own, NB this creates a partial
-- application, we can do better by compiling the builting in the
-- context of a call to it
compile _ _ (C.CoreBuiltin n) = App (Intrinsic $ intrinsicIndex n) mempty

-- | Compile primitive to STG native.
--
-- TODO: unify native handling
compile _ _ (C.CorePrim n) = case n of
  CoreInt i -> Atom (Literal (NativeInt i))
  CoreString s -> Atom (Literal (NativeString s))
  CoreSymbol s -> Atom (Literal (NativeSymbol s))
  CoreBoolean b -> Atom (Literal (NativeBool b))
  _ -> error "TODO: Unsupported native type"

-- | Block literals
compile envSize context (C.CoreBlock content) = let_ [c] b
  where
    c = compileBinding envSize context content
    cref = Local $ fromIntegral envSize
    b = appcon_ stgBlock [cref]

-- | Empty list
compile _ _ (C.CoreList []) = appcon_ stgNil mempty

-- | List literals
compile envSize context (C.CoreList els) = let_ elBinds buildList
  where
    elBinds = map (compileBinding envSize context) els
    elCount = length elBinds
    buildList =
      list_ (envSize + elCount) $ localsList envSize (envSize + elCount)

compile _ _ _ = undefined

emptyContext :: Eq v => v -> Ref
emptyContext _  = error "Missing from context during compilation"

extendContextForScope :: Int -> (v -> Ref) -> Int -> (Var Int v -> Ref)
extendContextForScope envSize context count = context'
  where
    context' (Var.B i) = newEnvRefs A.! fromIntegral (i + envSize)
    context' (Var.F x) = context x
    newEnvRefs =
      A.array
        (envSize, envSize + count - 1)
        [(i, Local $ fromIntegral i) | i <- [envSize .. envSize + count - 1]]
