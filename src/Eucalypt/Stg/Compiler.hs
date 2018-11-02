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
import Data.Bifunctor (first, second)
import Data.Foldable (toList)
import Data.List (nub, elemIndex, sortOn)
import Data.Scientific
import qualified Data.Vector as V
import Eucalypt.Core.Syn as C
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Globals
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- $ Building Blocks
--
-- Basic building blocks for compilation include constructor tags,
-- standard constructors, smart constructors and recipes.


-- | Construct a list as a STG LetRec
list_ :: Int -> [Ref] -> Maybe Ref -> StgSyn
list_ _ [] Nothing = Atom $ Global "KNIL"
list_ envSize [] (Just ref) =
  let_
    [pc0m_ ref $ value_ (Atom $ Global "KNIL")]
    (Atom $ Local $ fromIntegral envSize)
list_ envSize rs metaref = letrec_ pcs (Atom $ Local pcn)
  where
    preclose p v m = pcm_ [v, p] m consConstructor
    valrefs = reverse rs
    prevrefs = [Global "KNIL"] <> map (Local . fromIntegral) [envSize..]
    metarefs = replicate (length rs - 1) Nothing <> [metaref]
    pcs = zipWith3 preclose prevrefs valrefs metarefs
    pcn = fromIntegral $ envSize + length pcs - 1


-- | Convert a core primitive into a STG Native
convert :: C.Primitive -> Maybe Native
convert (CoreInt n) = Just $ NativeNumber $ fromIntegral n
convert (CoreFloat d) = Just $ NativeNumber $ fromFloatDigits d
convert (CoreSymbol s) = Just $ NativeSymbol s
convert (CoreString s) = Just $ NativeString s
convert (CoreBoolean b) = Just $ NativeBool b
convert CoreNull = Nothing


-- | Compile a let / letrec binding
--
compileBinding ::
     Eq v
  => Int -- ^ current environment size
  -> (v -> Ref) -- ^ current context (mapping vars to refs into environment)
  -> (CoreBindingName, C.CoreExp v) -- ^ annotation and expr to compile
  -> PreClosure
compileBinding _ context (nm, expr) = pc_ free $ compileLambdaForm expr
  where
    fvs = [(v, context v) | v <- nub . toList $ expr]
    free = map snd fvs
    context' v =
      maybe (context v) (Local . fromIntegral) (elemIndex v (map fst fvs))
    contextL' _ (Var.F v) = context' v
    contextL' envSize (Var.B i) = Local $ envSize + fromIntegral i
    compileLambdaForm e =
      case e of
        (CoreLambda smid _ ns body) ->
          lam_ (length free) (length ns) $
          ann_ nm smid $
          compile
            (length free + length ns)
            (contextL' . fromIntegral . length $ free)
            Nothing $
          fromScope body
        (CoreList _ []) -> nilConstructor -- TODO: id all std cons?
        CorePrim {} -> value_ $ compile (length free) context' Nothing e
        _ ->
          thunkn_ (length free) $
          ann_ nm (sourceMapId e) $ compile (length free) context' Nothing e


-- | Compile a CoreExp into STG expression
compile ::
     Eq v
  => Int -- ^ current environment size
  -> (v -> Ref) -- ^ current context (mapping vars to refs into environment)
  -> Maybe Ref -- ^ metadata as ref into environment
  -> C.CoreExp v -- ^ core expression to compile
  -> StgSyn

-- | Compile a let. All Core lets are potentially recursive
compile envSize context _metaref (C.CoreLet _ bs b) =
  letrec_ stgBindings stgBody
  where
    l = length bs
    envSize' = envSize + l
    stgBindings = map (compileBinding envSize' context' . second fromScope) bs
    stgBody = compile envSize' context' _metaref $ fromScope b
    context' = extendContextForScope envSize context l

-- | Compile a var
compile _ context _metaref (C.CoreVar _ v) = Atom $ context v

-- | Compile a builtin on its own, NB this creates a partial
-- application, we can do better by compiling the builtin in the
-- context of a call to it
compile _ _ _ (C.CoreBuiltin _ n) = App (Ref (Global n)) mempty

-- | Compile primitive to STG native.
compile _ _ _ (C.CorePrim _ p) = case convert p of
  Just n -> Atom (Literal n)
  Nothing -> Atom (Global "NULL")

-- | Block literals
compile envSize context _metaref (C.CoreBlock _ content) = let_ [c] b
  where
    c = compileBinding envSize context ("<content>", content)
    cref = Local $ fromIntegral envSize
    b = appcon_ stgBlock [cref]

-- | Empty list with metadata, allocate to embed metadta
compile envSize _ metaref (C.CoreList _ []) = list_ envSize [] metaref

-- | List literals
compile envSize context metaref (C.CoreList _ els) =
  if null elBinds
    then buildList
    else let_ elBinds buildList
  where
    elAnalyses = numberPreClosures (map classify els, 0)
    classify (CoreVar _ n) = EnvRef $ context n
    classify (CorePrim _ p) = EnvRef $ maybe (Global "NULL") Literal (convert p)
    classify expr = Closure Nothing expr
    elRefs = map (toRef envSize) elAnalyses
    elBinds =
      map (compileBinding envSize context . ("<item>", ) . closureExpr) $
      filter isClosure elAnalyses
    buildList = list_ (envSize + length elBinds) elRefs metaref

-- | Compile application, ensuring all args are atoms, allocating as
-- necessary to achieve this.
compile envSize context _metaref expr@C.CoreApply{} =
  compileApply envSize context  _metaref expr

-- | Compile lambda into let to allocate and return fn
compile envSize context _metaref f@CoreLambda{} =
  let_ [compileBinding envSize context ("<anon>", f)]
  $ Atom $ Local $ fromIntegral envSize

compile envSize context _metaref (CoreLookup _ obj nm) =
  let_
    [compileBinding envSize context ("", obj)]
    (appfn_
       (Global "LOOKUP")
       [Literal (NativeSymbol nm), Local (fromIntegral envSize)])

-- | Let allocate metadata and pass ref through to nested expression
-- for embedding in appropriate 'PreClosure'
compile envSize context _metaref (CoreMeta _ meta obj) =
  let_ [compileBinding envSize context ("", meta)] $
  compile (envSize + 1) context (Just (Local $ fromIntegral envSize)) obj

-- | Operator metadata no longer required by the time we hit STG, pass through
compile envSize context _ (CoreOperator _ _x _p expr) = compile envSize context Nothing expr

compile _ _ _ CoreName{} = error "Cannot compile name"
compile _ _ _ CoreArgTuple{} = error "Cannot compile arg tuple"
compile _ _ _ CoreOpSoup{} = error "Cannot compile op soup"
compile _ _ _ CoreEliminated = error "Cannot compile eliminated code"


data ArgCase a
  = EnvRef Ref
  | Scrutinee (Maybe Int)
              (CoreExp a)
  | Closure (Maybe Int)
            (CoreExp a)
  deriving (Show)

-- | Retrieve the index from an ArgCase (must be present)
index :: ArgCase a -> Int
index (Scrutinee (Just i) _) = i
index (Closure (Just i) _) = i
index _ = error "Unexpected argument case in CoreApply compilation"

toRef :: Int -> ArgCase a -> Ref
toRef envSize component = case component of
  EnvRef r -> r
  Scrutinee (Just n) _ -> Local $ fromIntegral (n + envSize)
  Closure (Just n) _ -> Local $ fromIntegral (n + envSize)
  _ -> error "Unexpected ArgCase during compilation"

closureExpr :: ArgCase a -> CoreExp a
closureExpr (Closure _ expr) = expr
closureExpr _ = error "closureExpr on non-closure"

isClosure :: ArgCase a -> Bool
isClosure (Closure _ _) = True
isClosure _ = False


-- | All arguments to apply must be primitives or vars (no complex
-- expressions). Depending on whether the argument position is strict
-- or non-strict, we generate wrapping case or let expressions to
-- pre-evaluate to values or allocate thunks.
compileApply ::
     Eq v
  => Int -- ^ current environment size
  -> (v -> Ref) -- ^ current context (mapping vars to refs into environment)
  -> Maybe Ref -- ^ metadata as ref into environment
  -> C.CoreExp v -- ^ core expression to compile
  -> StgSyn
compileApply envSize context metaref (C.CoreApply _ f xs) = wrappedCall
  where
    strictness =
      case op f of
        (CoreBuiltin _ n) -> standardGlobalStrictness n
        _ -> replicate (length xs) NonStrict
    components0 =
      case op f of
        (CoreBuiltin _ n) -> [EnvRef $ Global n]
        (CoreVar _ a) -> [EnvRef $ context a]
        _ -> [Closure Nothing f]
    acc comps (x, strict) =
      comps ++
      case x of
        (CoreVar _ a) ->
          case strict of
            NonStrict -> [EnvRef $ context a]
            Strict -> [Scrutinee Nothing x]
        (CorePrim _ n) -> [EnvRef $ maybe (Global "NULL") Literal (convert n)]
        _ ->
          case strict of
            NonStrict -> [Closure Nothing x]
            Strict -> [Scrutinee Nothing x]
    components = numberArgCases $ foldl acc components0 (zip xs strictness)
    call = compileCall envSize components
    wrappedCall =
      compileWrappers envSize context metaref (wrappers components) call
    op fn =
      case fn of
        (CoreOperator _ _x _p e) -> e
        _ -> fn
compileApply _ _ _ _ = error "compileApply called for non apply arg"


-- | Number the scrutinees (from 0), returning number cases plus next counter
numberScrutinees :: [ArgCase a] -> ([ArgCase a], Int)
numberScrutinees = first reverse . foldl acc ([], 0)
  where
    acc (o, n) i =
      case i of
        (Scrutinee Nothing x) -> (Scrutinee (Just n) x : o, n + 1)
        _ -> (i : o, n)



-- | Number the pre closures (from next counter), returning numbered cases
numberPreClosures :: ([ArgCase a], Int) -> [ArgCase a]
numberPreClosures (cs, next) = reverse $ fst $ foldl acc ([], next) cs
  where
    acc (o, n) i =
      case i of
        (Closure Nothing x) -> (Closure (Just n) x : o, n + 1)
        _ -> (i : o, n)



numberArgCases :: [ArgCase a] -> [ArgCase a]
numberArgCases = numberPreClosures . numberScrutinees



-- | Sort those args which need evaluating or allocating into
-- scrutinees then preclosures in the order dictated by numbering
wrappers :: [ArgCase a] -> ([ArgCase a], [ArgCase a])
wrappers = span isScrutinee . sortOn index . filter notEnvRef
  where
    notEnvRef (EnvRef _) = False
    notEnvRef _ = True
    isScrutinee Scrutinee{} = True
    isScrutinee _ = False



-- | Compile the apply expression
compileCall :: Int -> [ArgCase a] -> StgSyn
compileCall envSize components =
  App (Ref $ toRef envSize $ head components) $
  V.fromList (map (toRef envSize) $ tail components)


-- | Compile the wrappers
compileWrappers ::
     Eq v
  => Int
  -> (v -> Ref)
  -> Maybe Ref
  -> ([ArgCase v], [ArgCase v])
  -> StgSyn
  -> StgSyn
compileWrappers envSize  context metaref (scrutinees, closures) call =
  foldr wrapCase allocWrappedCall scrutinees
  where
    preclosures =
      map (compileBinding envSize context . ("", ) . closureExpr) closures
    allocWrappedCall =
      if null preclosures
        then call
        else let_ preclosures call
    wrapCase c body =
      case c of
        (Scrutinee (Just n) expr) ->
          force_ (compile (envSize + n) context metaref expr) body
        _ ->
          error "Unexpected component while wrapping cases in apply compilation"



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
    [pc0_ $ thunk_ (compile 0 emptyContext Nothing expr)]
    (appfn_ (Global "RENDER") [Local 0])
