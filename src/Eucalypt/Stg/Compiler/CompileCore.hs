{-# LANGUAGE RecursiveDo #-}
{-|
Module      : Eucalypt.Stg.Compiler.CompileCore
Description : Compile core to STG-code
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Compiler.CompileCore where

import Bound.Scope (fromScope)
import Data.Foldable (foldrM, toList)
import Data.List (nub)
import Eucalypt.Core.Syn as C
import Eucalypt.Core.Pretty
import Eucalypt.Stg.Compiler.Application
import Eucalypt.Stg.Compiler.Common
import Eucalypt.Stg.Compiler.Context
import Eucalypt.Stg.Compiler.LetBinder
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Intrinsics
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags


-- | How we compile a binding may depend on whether it has been
-- explicitly bound in Core and so may need to be shareable or whether
-- it is an implied intermediate needed by a complex expression.
data BindReason = Explicit | Implicit



-- | The very common case is we are compiling expressions which are
-- the body of a let binding. In this case we have opportunities to
-- merge bindings in the compilee up into the bindings of the parent.
--
compileBinding ::
     (Eq v, Show v)
  => Context v
  -> Maybe Ref
  -> Maybe CoreBindingName
  -> BindReason
  -> CoreExp v
  -> LetBinder Ref



-- | Compile a lambda form as a binding of a surrounding letrec
--
-- TODO: cheap let floating?
-- TODO: respect metadata
compileBinding context _metaref name _ expr@(CoreLambda smid _ ns body) = do
  let pc =
        pc_ freeRefs $
        lam_ freec argc $
        annotate name smid $
        compile
          lamContext
          Nothing
          (fromScope body)
          (freec + argc)
  addBinding pc
  where
    (freeVars, freeRefs) = unzip [(v, context v) | v <- nub . toList $ expr]
    freec = length freeRefs
    argc = length ns
    freeContext = toContext freeVars
    lamContext = lambdaContext freec freeContext



-- | Compile a subordinate letrec binding as bindings of the parent
-- letrec.
--
-- This has some circular programming magic. The references returned
-- by compiling the subordinate bindings are fed into the compilation
-- which returns them.
compileBinding context metaref name _ (CoreLet _ bs body _) = do
  rec rs <- traverse (bind rs) bs
  bindBody rs
  where
    bind rs (n, b) =
      compileBinding
        (extendContextForEmbeddedScope context (refs rs))
        Nothing
        (Just n)
        Explicit
        (fromScope b)
    bindBody rs =
      compileBinding
        (extendContextForEmbeddedScope context (refs rs))
        metaref
        name
        Implicit
        (fromScope body)



-- | Vars already have refs in the context which can be used
compileBinding context _metaref _name Implicit (CoreVar _ v) =
  return $ context v



-- | For explicit sharing, bind Atom
compileBinding context _metaref _name Explicit (CoreVar _ v) =
  addBinding $ pc_ [context v] $ valuen_ 1 $ Atom $ L 0



-- | Builtins are refs into global environment
compileBinding _context _metaref _name Implicit (CoreBuiltin _ i) =
  return $ gref i



-- | For explicit sharing, partially call
compileBinding _context _metaref _name Explicit (CoreBuiltin _ i) =
  addBinding $ pc0_ $ thunk_ $ appfn_ (gref i) []



-- | Primitives can be converted directly into native or global refs
compileBinding _context Nothing _name Implicit (CorePrim _ p) =
  return $ literal p



-- | For shareable primitives allocate an atom (hopefully these have
-- all been inlined out)
compileBinding _context Nothing _name Explicit (CorePrim _ p) =
  addBinding $ pc0_ $ value_ $ Atom (literal p)



-- | Primitives with meta data need allocating though
compileBinding _context (Just metaref) _name _ (CorePrim _ p) =
  addBinding $
  pc_ [metaref] $
  thunkn_ 1 $ appbif_ (intrinsicIndex "WITHMETA") [L 0, literal p]



-- | The empty list has a global binding
compileBinding _context Nothing _name Implicit (CoreList _ []) =
  return $ gref "KNIL"



-- | Allocate an atom for sharing
compileBinding _context Nothing _name Explicit (CoreList _ []) =
  addBinding $ pc0_ $ value_ $ Atom $ gref "KNIL"



-- | Embed the list bindings in current bindings TODO: meta
compileBinding context Nothing _name _ (CoreList _ xs) = do
  rs <- traverse (compileBinding context Nothing Nothing Implicit) xs
  linkAll rs
  where
    link v t = addBinding $ pc_ [v, t] consConstructor
    linkAll = foldrM link (gref "KNIL")



-- | Embed the list bindings in current bindings TODO: meta
compileBinding context metaref _name _ (CoreList _ xs) = do
  rs <- traverse (compileBinding context Nothing Nothing Implicit) xs
  let ms = [metaref] <> repeat Nothing
  linkAllWithMeta rs ms
  where
    link (v, m) t = addBinding $ pcm_ [v, t] m consConstructor
    linkAllWithMeta rs ms = foldrM link (gref "KNIL") (zip rs ms)



-- | The empty block has a global binding
compileBinding _context Nothing _name Implicit (CoreBlock _ (CoreList _ [])) =
  return $ gref "KEMPTYBLOCK"



-- | Allocate an atom for sharing
compileBinding _context Nothing _name Explicit (CoreBlock _ (CoreList _ [])) =
  addBinding $ pc0_ $ value_ $ Atom $ gref "KEMPTYBLOCK"



-- | Empbed all the data structure bindings
compileBinding context metaref _name _ (CoreBlock _ content) = do
  lst <- compileBinding context Nothing Nothing Implicit content
  let (env, [r]) = sortRefs [lst]
  addBinding $ pcm_ env metaref $ valuen_ (length env) $ appcon_ stgBlock [r]



-- | Simple lookup without default, binds object if necessary
compileBinding context _metaref _name _ (CoreLookup _ obj nm Nothing) = do
  objr <- compileBinding context Nothing Nothing Implicit obj
  let (env, [r]) = sortRefs [objr]
  addBinding $
    pc_ env $
    thunkn_ (length env) $ appfn_ (gref "LOOKUP") [V (NativeSymbol $ intern nm), r]


-- | Lookup without default, binds object and default if necessary
compileBinding context _metaref _name _ (CoreLookup _ obj nm (Just deft)) = do
  objr <- compileBinding context Nothing Nothing Implicit obj
  deftr <- compileBinding context Nothing Nothing Implicit deft
  let (env, [obr, dr]) = sortRefs [objr, deftr]
  addBinding $
    pc_ env $
    thunkn_ (length env) $
    appfn_ (gref "LOOKUPOR") [V (NativeSymbol $ intern nm), dr, obr]


-- | Compile non-strict bindings into subsuming let, then cases inside
--
-- TODO: meta
compileBinding context _metaref _name _ (C.CoreApply _ f xs) =
  traverse compileNonStrict (analyse2 f xs) >>=
  addBinding . precloseWrappedCall
  where
    comp = compileBinding context Nothing Nothing Implicit
    compileNonStrict = mapClosureExpr $ fmap EnvRef . comp
    -- formulate case-wrapped call (once env size, 'es' is available)
    precloseWrappedCall acs = pc_ pcrs $ thunkn_ pcrc wrappedCall
      where
        (acs', _) = numberScrutinees (acs, 0) -- with increments over es
        (argFreeRefs, acs'') = sortArgCaseRefs acs'
        scrutineeFreeRefs = freeScrutineeRefs context acs''
        pcrs = argFreeRefs <> scrutineeFreeRefs
        pcrc = length pcrs
        scrutineeContext = closureContext pcrs context
        call = compileCall pcrc acs''
        wrappedCall = foldr wrapCase call acs''
        wrapCase (Scrutinee (Just i) expr) body =
          force_ (compile scrutineeContext Nothing expr (pcrc + i)) body
        wrapCase _ body = body


-- | Bind meta and forward on with the reference
--
-- TODO: metdata merge
compileBinding context _metaref _name _ (C.CoreMeta _ meta obj) = do
  r <- compileBinding context Nothing Nothing Implicit meta
  compileBinding context (Just r) Nothing Implicit obj



-- | Pass right through fixity metadata
compileBinding context metaref name reason (CoreOperator _ _ _ expr) =
  compileBinding context metaref name reason expr




compileBinding _context _metaref _name _reason expr =  error $ "undefined compileBinding:" ++ pprint expr



-- | When compiling in a context where we don't have a surrounding
-- letrec to contribute bindings to we create new contexts and return
-- standalone syntax.
compileBody :: (Eq v, Show v) => Context v -> Maybe Ref -> CoreExp v -> LetBinder (Int -> StgSyn)


-- | Compile a let. TODO: the body shouldn't be a binding
compileBody context metaref (C.CoreLet _ bs body _) = do
  rec rs <- traverse (bind rs) bs
  compBody rs
  where
    bind rs (n, b) =
      compileBinding
        (extendContextForEmbeddedScope context (refs rs))
        Nothing
        (Just n)
        Explicit
        (fromScope b)
    compBody rs =
      compileBody
        (extendContextForEmbeddedScope context (refs rs))
        metaref
        (fromScope body)




compileBody context _metaref (CoreBlock _ content) = do
  lst <- compileBinding context Nothing Nothing Implicit content
  return . const $ appcon_ stgBlock [lst]




compileBody context _metaref (CoreLookup _ obj nm Nothing) = do
  objr <- compileBinding context Nothing Nothing Implicit obj
  return . const $ appfn_ (gref "LOOKUP") [V (NativeSymbol $ intern nm), objr]



compileBody context _metaref (CoreLookup _ obj nm (Just deft)) = do
  objr <- compileBinding context Nothing Nothing Implicit obj
  deftr <- compileBinding context Nothing Nothing Implicit deft
  return . const $ appfn_ (gref "LOOKUPOR") [V (NativeSymbol $ intern nm), deftr, objr]



-- cases as the body.
compileBody context _metaref (C.CoreApply _ f xs) =
  wrapCall <$> traverse compileNonStrict (analyse2 f xs)
  where
    comp = compileBinding context Nothing Nothing Implicit
    compileNonStrict = mapClosureExpr $ fmap EnvRef . comp
    wrapCall acs es =
      let (acs', _) = numberScrutinees (acs, 0)
          call = compileCall es acs'
          wrapCase (i, expr) = force_ (compile context Nothing expr (es + i))
       in foldr wrapCase call (scrutinees acs')



-- | Bind meta and forward on with the reference
compileBody context _metaref (C.CoreMeta _ meta obj) = do
  r <- compileBinding context Nothing Nothing Implicit meta
  compileBody context (Just r) obj



-- | Pass right through fixity metadata
compileBody context metaref (CoreOperator _ _ _ expr) =
  compileBody context metaref expr



-- | We can always default to generating all bindings a referencing
-- the last in the body
compileBody context metaref expr = do
  r <- compileBinding context metaref Nothing Implicit expr
  return . const . Atom $ r



-- | Create a letrec context and forward to compileBody /
-- compileBinding
compile :: (Eq v, Show v) => Context v -> Maybe Ref -> CoreExp v -> Int -> StgSyn
compile context metaref expr envBase =
  let (completeBody, pcs) =
        runLetBinder (compileBody context metaref expr) envBase
      body = completeBody (envBase + length pcs)
   in if null pcs
        then body
        else letrec_ pcs body



-- | Wrap in a render builtin which will NF eval and emit render events
compileForRender :: CoreExpr -> StgSyn
compileForRender expr =
  let_
    [pc0_ $ thunk_ (compile emptyContext Nothing expr 0)]
    (appfn_ (gref "RENDER") [L 0])
