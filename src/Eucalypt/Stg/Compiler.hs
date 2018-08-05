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

import qualified Data.Vector as Vector
import Eucalypt.Stg.Syn

-- $ Building Blocks
--
-- Basic building blocks for compilation include constructor tags,
-- standard constructors, smart constructors and recipes.

stgNil :: Tag
stgNil = 0

nilConstructor :: LambdaForm
nilConstructor = standardConstructor 0 stgNil

stgCons :: Tag
stgCons = 1

consConstructor :: LambdaForm
consConstructor = standardConstructor 2 stgCons

stgBlock :: Tag
stgBlock = 2

blockConstructor :: LambdaForm
blockConstructor = standardConstructor 1 stgBlock

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

litList_ :: [Native] -> StgSyn
litList_ nats = letrec_ (pc0 : pcs) (App (Ref (Local pcn)) mempty)
  where
    pc0 = PreClosure mempty nilConstructor
    preclose (i, n) =
      PreClosure (Vector.fromList [Literal n, Local i]) consConstructor
    pcs = zipWith (curry preclose) [0 ..] $ reverse nats
    pcn = fromIntegral $ length pcs - 1

-- | Compile an expression (which may appear in argument position in
-- core) into a binding for an STG let.
-- compileBinding :: Eq v => (v -> Ref) -> (C.CoreExp v) -> PreClosure
-- compileBinding context code =
--   case code
--     -- | Bound vars become stack refs, existing free vars that are
--     -- stack refs are shifted to after the args (?) and the closure
--     -- will not be updateable
--         of
--     (C.CoreLambda names scope) ->
--       PreClosure fvv $
--       noUpdate fvn arityW (compile arity context' $ fromScope scope)
--       where context' (Var.F v) =
--               case context v of
--                 (Stack n) -> Stack (n + arityW)
--                 r -> r
--             context' (Var.B n) = (Stack $ fromIntegral n)
--             arity = length names
--             arityW = fromIntegral arity
--     -- | Any other expression does not impact stack arrangement (?)
--     -- and generates a thunk
--     _ -> PreClosure fvv $ doUpdate fvn (compile 0 context code)
--   where
--     fvs =
--       filter (isContextualRef . snd) .
--       map (\v -> (v, context v)) . nub . toList $
--       code
--     fvn = fromIntegral $ length fvs
--     fvv = Vector.Vector.fromList (map snd fvs)

-- compileArgs :: Eq v => (v -> Ref) -> [C.CoreExp v] -> ([Ref], Int, [PreClosure])
-- compileArgs context xs = undefined

-- -- | Compile a Core application into STG. All args must be simple refs
-- -- so we generate bindings for them.
-- compileApp :: Eq v => Int -> (C.CoreExp v) -> [C.CoreExp v] -> StgSyn
-- compileApp n f xs = undefined -- App n (compile)

-- compile :: Eq v => Int -> (v -> Ref) -> (C.CoreExp v) -> StgSyn
-- compile n context l@C.CoreLambda {} =
--   Let
--     (Vector.singleton (compileBinding context l))
--     (App (fromIntegral n + 1) (Ref $ Stack 0) mempty)
-- compile n context (C.CoreLet bs b) =
--   LetRec (Vector.Vector.fromList bs') (compile (n + l) context' $ fromScope b)
--   where
--     l = length bs
--     context' (Var.F v) =
--       case context v of
--         (Stack i) -> Stack (i + fromIntegral l)
--         r -> r
--     context' (Var.B i) = Stack $ fromIntegral i
--     bs' = map (compileBinding context' . fromScope . snd) bs
-- compile n context (C.CoreList els) =
--   case els of
--     [] -> let con = standardConstructor 0 stgNil
--           in Let (Vector.singleton $ PreClosure mempty con)
--              (App (fromIntegral n + 1) (Ref $ Stack 0) mempty)
--     x:xs -> let (refs, k, pcs) = compileArgs context [x, C.CoreList xs]
--                 con = standardConstructor (fromIntegral $ length refs) stgCons
--             in Let (Vector.Vector.fromList $ pcs ++ [PreClosure (Vector.Vector.fromList refs) con])
--                      (App (fromIntegral n + fromIntegral k + 1) (Ref $ Stack $ fromIntegral k) mempty)
