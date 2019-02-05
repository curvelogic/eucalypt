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
import Eucalypt.Core.Syn as C
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics (intrinsicIndex)
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

-- $ Building Blocks
--
-- Basic building blocks for compilation include constructor tags,
-- standard constructors, smart constructors and recipes.


-- | Construct a list as a STG LetRec
list_ :: Int -> [Ref] -> Maybe Ref -> StgSyn
list_ _ [] Nothing = Atom $ gref "KNIL"
list_ envSz [] (Just ref) =
  let_
    [pc0m_ ref $ value_ (Atom $ gref "KNIL")]
    (Atom $ L $ fromIntegral envSz)
list_ envSz rs metaref = letrec_ pcs (Atom $ L pcn)
  where
    preclose p v m = pcm_ [v, p] m consConstructor
    valrefs = reverse rs
    prevrefs = [gref "KNIL"] <> map (L . fromIntegral) [envSz..]
    metarefs = replicate (length rs - 1) Nothing <> [metaref]
    pcs = zipWith3 preclose prevrefs valrefs metarefs
    pcn = fromIntegral $ envSz + length pcs - 1


-- | Convert a core primitive into a STG Native
convert :: C.Primitive -> Ref
convert (CoreInt n) = V $ NativeNumber $ fromIntegral n
convert (CoreFloat d) = V $ NativeNumber $ fromFloatDigits d
convert (CoreSymbol s) = V $ NativeSymbol s
convert (CoreString s) = V $ NativeString s
convert (CoreBoolean True) = gref "TRUE"
convert (CoreBoolean False) = gref "FALSE"
convert CoreNull = gref "NULL"


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
      maybe (context v) (L . fromIntegral) (elemIndex v (map fst fvs))
    contextL' _ (Var.F v) = context' v
    contextL' envSz (Var.B i) = L $ envSz + fromIntegral i
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
compile envSz context _metaref (C.CoreLet _ bs b _) =
  letrec_ stgBindings stgBody
  where
    l = length bs
    envSz' = envSz + l
    stgBindings = map (compileBinding envSz' context' . second fromScope) bs
    stgBody = compile envSz' context' _metaref $ fromScope b
    context' = extendContextForScope envSz context l

-- | Compile a var
compile _ context _metaref (C.CoreVar _ v) = Atom $ context v

-- | Compile a builtin on its own, NB this creates a partial
-- application, we can do better by compiling the builtin in the
-- context of a call to it
compile _ _ _ (C.CoreBuiltin _ n) = App (Ref (gref n)) mempty

-- | Compile primitive to STG native.
compile _ _context metaref (C.CorePrim _ p) = annotated $ convert p
  where
    annotated rf =
      maybe
        (Atom rf)
        (\r -> appbif_ (intrinsicIndex "WITHMETA") [r, rf])
        metaref

-- | Block literals
compile envSz context _metaref (C.CoreBlock _ content) = let_ [c] b
  where
    c = compileBinding envSz context ("<content>", content)
    cref = L $ fromIntegral envSz
    b = appcon_ stgBlock [cref]

-- | Empty list with metadata, allocate to embed metadta
compile envSz _ metaref (C.CoreList _ []) = list_ envSz [] metaref

-- | List literals
compile envSz context metaref (C.CoreList _ els) =
  if null elBinds
    then buildList
    else let_ elBinds buildList
  where
    elAnalyses = numberPreClosures (map classify els, 0)
    classify (CoreVar _ n) = EnvRef $ context n
    classify (CorePrim _ p) = EnvRef $ convert p
    classify expr = Closure Nothing expr
    elRefs = map (toRef envSz) elAnalyses
    elBinds =
      map (compileBinding envSz context . ("<item>", ) . closureExpr) $
      filter isClosure elAnalyses
    buildList = list_ (envSz + length elBinds) elRefs metaref

-- | Compile application, ensuring all args are atoms, allocating as
-- necessary to achieve this.
compile envSz context _metaref expr@C.CoreApply{} =
  compileApply envSz context  _metaref expr

-- | Compile lambda into let to allocate and return fn
compile envSz context _metaref f@CoreLambda{} =
  let_ [compileBinding envSz context ("<anon>", f)]
  $ Atom $ L $ fromIntegral envSz

-- | Compile a dot lookup or name used in a generalised lookup context
compile envSz context _metaref (CoreLookup _ obj nm deft) =
  case deft of
    Nothing ->
      let_
        [compileBinding envSz context ("", obj)]
        (appfn_
           (gref "LOOKUP")
           [V (NativeSymbol nm), L (fromIntegral envSz)])
    (Just expr) ->
      let_
        [ compileBinding envSz context ("", expr)
        , compileBinding envSz context ("", obj)
        ]
        (appfn_
           (gref "LOOKUPOR")
           [ V (NativeSymbol nm)
           , L (fromIntegral envSz)
           , L (fromIntegral envSz + 1)
           ])


-- | Let allocate metadata and pass ref through to nested expression
-- for embedding in appropriate 'PreClosure'
compile envSz context _metaref (CoreMeta _ meta obj) =
  let_ [compileBinding envSz context ("", meta)] $
  compile (envSz + 1) context (Just (L $ fromIntegral envSz)) obj

-- | Operator metadata no longer required by the time we hit STG, pass through
compile envSz context _ (CoreOperator _ _x _p expr) = compile envSz context Nothing expr

compile _ _ _ CoreName{} = error "Cannot compile name"
compile _ _ _ CoreArgTuple{} = error "Cannot compile arg tuple"
compile _ _ _ CoreOpSoup{} = error "Cannot compile op soup"
compile _ _ _ CoreUnresolved{} = error "Cannot compile unresolved"
compile _ _ _ CoreEliminated = error "Cannot compile eliminated code"


data ArgCase a
  = EnvRef Ref
  | Scrutinee (Maybe Int)
              (CoreExp a)
  | Closure (Maybe Int)
            (CoreExp a)
  deriving (Show)

-- | Retrieve the index from an ArgCase (must be present)
argCaseindex :: ArgCase a -> Int
argCaseindex (Scrutinee (Just i) _) = i
argCaseindex (Closure (Just i) _) = i
argCaseindex _ = error "Unexpected argument case in CoreApply compilation"

toRef :: Int -> ArgCase a -> Ref
toRef envSz component = case component of
  EnvRef r -> r
  Scrutinee (Just n) _ -> L $ fromIntegral (n + envSz)
  Closure (Just n) _ -> L $ fromIntegral (n + envSz)
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
compileApply envSz context metaref (C.CoreApply _ f xs) = wrappedCall
  where
    strictness =
      case op f of
        (CoreBuiltin _ n) -> globalSignature n
        _ -> replicate (length xs) NonStrict
    components0 =
      case op f of
        (CoreBuiltin _ n) -> [EnvRef $ gref n]
        (CoreVar _ a) -> [EnvRef $ context a]
        _ -> [Closure Nothing f]
    acc comps (x, strict) =
      comps ++
      case x of
        (CoreVar _ a) ->
          case strict of
            NonStrict -> [EnvRef $ context a]
            Strict -> [Scrutinee Nothing x]
        (CorePrim _ n) -> [EnvRef $ convert n]
        _ ->
          case strict of
            NonStrict -> [Closure Nothing x]
            Strict -> [Scrutinee Nothing x]
    components = numberArgCases $ foldl acc components0 (zip xs strictness)
    call = compileCall envSz components
    wrappedCall =
      compileWrappers envSz context metaref (wrappers components) call
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
wrappers = span isScrutinee . sortOn argCaseindex . filter notEnvRef
  where
    notEnvRef (EnvRef _) = False
    notEnvRef _ = True
    isScrutinee Scrutinee{} = True
    isScrutinee _ = False



-- | Compile the apply expression
compileCall :: Int -> [ArgCase a] -> StgSyn
compileCall envSz components =
  App (Ref $ toRef envSz $ head components) $
  refs (map (toRef envSz) $ tail components)


-- | Compile the wrappers
compileWrappers ::
     Eq v
  => Int
  -> (v -> Ref)
  -> Maybe Ref
  -> ([ArgCase v], [ArgCase v])
  -> StgSyn
  -> StgSyn
compileWrappers envSz  context metaref (scrutinees, closures) call =
  foldr wrapCase allocWrappedCall scrutinees
  where
    preclosures =
      map (compileBinding envSz context . ("", ) . closureExpr) closures
    allocWrappedCall =
      if null preclosures
        then call
        else let_ preclosures call
    wrapCase c body =
      case c of
        (Scrutinee (Just n) expr) ->
          force_ (compile (envSz + n) context metaref expr) body
        _ ->
          error "Unexpected component while wrapping cases in apply compilation"



-- | An empty context with no Refs for any Var
emptyContext :: (Show v, Eq v) => v -> Ref
emptyContext v  = error $ show v ++ " missing from context during compilation"

-- | Extend the context to apply to Vars in Scope
extendContextForScope :: Int -> (v -> Ref) -> Int -> (Var Int v -> Ref)
extendContextForScope envSz context count = context'
  where
    context' (Var.B i) = newEnvRefs A.! fromIntegral (i + envSz)
    context' (Var.F x) = context x
    newEnvRefs =
      A.array
        (envSz, envSz + count - 1)
        [(i, L $ fromIntegral i) | i <- [envSz .. envSz + count - 1]]

-- | Wrap in a render builtin which will NF eval and emit render events
compileForRender :: CoreExpr -> StgSyn
compileForRender expr =
  let_
    [pc0_ $ thunk_ (compile 0 emptyContext Nothing expr)]
    (appfn_ (gref "RENDER") [L 0])
