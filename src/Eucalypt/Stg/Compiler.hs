{-# LANGUAGE TupleSections #-}
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
import Data.Bifunctor (second)
import Data.Foldable (toList)
import Data.List (nub, elemIndex)
import Data.Scientific
import qualified Data.Vector as V
import Eucalypt.Core.Syn as C
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- $ Building Blocks
--
-- Basic building blocks for compilation include constructor tags,
-- standard constructors, smart constructors and recipes.


-- | Construct a list asa STG LetRec
list_ :: Int -> [Ref] -> StgSyn
list_ _ [] = appcon_ stgNil mempty
list_ envSize rs = letrec_ (pc0 : pcs) (App (Ref (Local pcn)) mempty)
  where
    pc0 = pc0_ nilConstructor
    preclose (i, r) = pc_ [r, Local $ fromIntegral i] consConstructor
    pcs = zipWith (curry preclose) [envSize ..] $ reverse rs
    pcn = fromIntegral $ envSize + length pcs

convert :: C.Primitive -> Maybe Native
convert (CoreInt n) = Just $ NativeNumber $ fromIntegral n
convert (CoreFloat d) = Just $ NativeNumber $ fromFloatDigits d
convert (CoreSymbol s) = Just $ NativeSymbol s
convert (CoreString s) = Just $ NativeString s
convert (CoreBoolean b) = Just $ NativeBool b
convert CoreNull = Nothing


-- | Compile a let / letrec binding
compileBinding :: Eq v => Int -> (v -> Ref) -> (CoreBindingName, C.CoreExp v) -> PreClosure
compileBinding _ context (nm, expr) = pc_ free $ compileLambdaForm expr
  where
    fvs = [(v, context v) | v <- nub . toList $ expr]
    free = map snd fvs
    context' v =
      maybe (context v) (Local . fromIntegral) (elemIndex v (map fst fvs))
    contextL' (Var.F v) = context' v
    contextL' (Var.B i) = BoundArg $ fromIntegral i
    compileLambdaForm e =
      case e of
        (CoreLambda ns body) ->
          lam_ (length free) (length ns) $ ann_ nm $
          compile (length free) contextL' $ fromScope body
        (CoreList []) -> nilConstructor -- TODO: id all std cons?
        CorePrim {} -> value_ $ compile (length free) context' e
        _ -> thunkn_ (length free) $ compile (length free) context' e


-- | Compile a CoreExp into STG expression
compile :: Eq v => Int -> (v -> Ref) -> C.CoreExp v -> StgSyn

-- | Compile a let. All Core lets are potentially recursive
compile envSize context (C.CoreLet bs b) = letrec_ stgBindings stgBody
  where
    l = length bs
    envSize' = envSize + l
    stgBindings = map (compileBinding envSize' context' . second fromScope) bs
    stgBody = compile envSize' context' $ fromScope b
    context' = extendContextForScope envSize context l

-- | Compile a var
compile _ context (C.CoreVar v) = Atom $ context v

-- | Compile a builtin on its own, NB this creates a partial
-- application, we can do better by compiling the builtin in the
-- context of a call to it
compile _ _ (C.CoreBuiltin n) = App (Ref (Global n)) mempty

-- | Compile primitive to STG native.
--
-- TODO: unify native handling
compile _ _ (C.CorePrim p) = case convert p of
  Just n -> Atom (Literal n)
  Nothing -> Atom (Global "NULL")

-- | Block literals
compile envSize context (C.CoreBlock content) = let_ [c] b
  where
    c = compileBinding envSize context ("", content)
    cref = Local $ fromIntegral envSize
    b = appcon_ stgBlock [cref]

-- | Empty list
compile _ _ (C.CoreList []) = appcon_ stgNil mempty

-- | List literals
compile envSize context (C.CoreList els) = let_ elBinds buildList
  where
    elBinds = map (compileBinding envSize context . ("",)) els
    elCount = length elBinds
    buildList =
      list_ (envSize + elCount) $ localsList envSize (envSize + elCount)


-- | Compile application, ensuring all args are atoms, allocating as
-- necessary to achieve this.
compile envSize context (C.CoreApply f xs) =
  if null pcs
    then App func $ V.fromList xrefs
    else let_ pcs (App func $ V.fromList xrefs)
  where
    (pcs0, func) =
      case op f of
        (CoreBuiltin n) -> ([], Ref $ Global n)
        (CoreVar a) -> ([], Ref $ context a)
        _ ->
          ( [compileBinding envSize context ("<fn>", f)]
          , Ref (Local $ fromIntegral envSize))
    acc (ps, xrs) x =
      case x of
        (CoreVar a) -> (ps, xrs ++ [context a])
        (CorePrim n) -> (ps, xrs ++ [maybe (Global "NULL") Literal (convert n)])
        _ ->
          ( ps ++ [compileBinding envSize context ("", x)]
          , xrs ++ [Local $ fromIntegral $ envSize + length ps])
    (pcs, xrefs) = foldl acc (pcs0, []) xs
    op fn =
      case fn of
        (CoreOperator _x _p e) -> e
        _ -> fn


compile envSize context (CoreLookup obj nm) =
  let_
    [compileBinding envSize context ("", obj)]
    (appfn_ (Global "LOOKUP") [Literal (NativeSymbol nm)])


-- | TODO: implement metadata in STG
compile envSize context (CoreMeta _meta obj) = compile envSize context obj

-- | Operator metadata no longer required by the time we hit STG, pass through
compile envSize context (CoreOperator _x _p expr) = compile envSize context expr

compile _ _ (CoreName _) = error "Cannot compile name"
compile _ _ (CoreArgTuple _) = error "Cannot compile arg tuple"
compile _ _ (CoreOpSoup _) = error "Cannot compile op soup"
compile _ _ (CoreLambda _ _) = error "Cannot compile lambda"
compile _ _ CorePAp{} = error "Cannot compile PAp"
compile _ _ (CoreTraced _) = error "Cannot compile traced"
compile _ _ (CoreChecked _ _) = error "Cannot compile checked"

-- | An empty context with no Refs for any Var
emptyContext :: (Show v, Eq v) => v -> Ref
emptyContext v  = error $ show v ++ " missing from context during compilation"

-- | Extend the context to apply to Vars in Scope
extendContextForScope :: Int -> (v -> Ref) -> Int -> (Var Int v -> Ref)
extendContextForScope envSize context count = context'
  where
    context' (Var.B i) = newEnvRefs A.! fromIntegral (i + envSize)
    context' (Var.F x) = context x
    newEnvRefs =
      A.array
        (envSize, envSize + count - 1)
        [(i, Local $ fromIntegral i) | i <- [envSize .. envSize + count - 1]]

-- | Wrap in a render builtin which will NF eval and emit render events
compileForRender :: CoreExpr -> StgSyn
compileForRender expr =
  let_
    [pc0_ $ thunk_ (compile 0 emptyContext expr)]
    (appfn_ (Global "RENDER") [Local 0])
