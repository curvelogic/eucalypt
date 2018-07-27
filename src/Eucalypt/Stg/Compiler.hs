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

-- import Bound
-- import Bound.Var as Var
-- import Data.List (nub)
-- import qualified Data.Vector as Vector
-- import Data.Foldable (toList)
-- import qualified Eucalypt.Core.Syn as C
-- import qualified Eucalypt.Stg.Syn as S
-- type GlobalId = String
-- data Program = Program [(GlobalId, S.LambdaForm)]


-- | Compile an expression (which may appear in argument position in
-- core) into a binding for an STG let.
-- compileBinding :: Eq v => (v -> S.Ref) -> (C.CoreExp v) -> S.PreClosure
-- compileBinding context code =
--   case code
--     -- | Bound vars become stack refs, existing free vars that are
--     -- stack refs are shifted to after the args (?) and the closure
--     -- will not be updateable
--         of
--     (C.CoreLambda names scope) ->
--       S.PreClosure fvv $
--       S.noUpdate fvn arityW (compile arity context' $ fromScope scope)
--       where context' (Var.F v) =
--               case context v of
--                 (S.Stack n) -> S.Stack (n + arityW)
--                 r -> r
--             context' (Var.B n) = (S.Stack $ fromIntegral n)
--             arity = length names
--             arityW = fromIntegral arity
--     -- | Any other expression does not impact stack arrangement (?)
--     -- and generates a thunk
--     _ -> S.PreClosure fvv $ S.doUpdate fvn (compile 0 context code)
--   where
--     fvs =
--       filter (S.isContextualRef . snd) .
--       map (\v -> (v, context v)) . nub . toList $
--       code
--     fvn = fromIntegral $ length fvs
--     fvv = Vector.fromList (map snd fvs)

-- compileArgs :: Eq v => (v -> S.Ref) -> [C.CoreExp v] -> ([S.Ref], Int, [S.PreClosure])
-- compileArgs context xs = undefined

-- -- | Compile a Core application into STG. All args must be simple refs
-- -- so we generate bindings for them.
-- compileApp :: Eq v => Int -> (C.CoreExp v) -> [C.CoreExp v] -> S.StgSyn
-- compileApp n f xs = undefined -- S.App n (compile)

-- stgNil :: S.Tag
-- stgNil = 0

-- stgCons :: S.Tag
-- stgCons = 1

-- stgBlock :: S.Tag
-- stgBlock = 2

-- compile :: Eq v => Int -> (v -> S.Ref) -> (C.CoreExp v) -> S.StgSyn
-- compile n context l@C.CoreLambda {} =
--   S.Let
--     (Vector.singleton (compileBinding context l))
--     (S.App (fromIntegral n + 1) (S.Ref $ S.Stack 0) mempty)
-- compile n context (C.CoreLet bs b) =
--   S.LetRec (Vector.fromList bs') (compile (n + l) context' $ fromScope b)
--   where
--     l = length bs
--     context' (Var.F v) =
--       case context v of
--         (S.Stack i) -> S.Stack (i + fromIntegral l)
--         r -> r
--     context' (Var.B i) = S.Stack $ fromIntegral i
--     bs' = map (compileBinding context' . fromScope . snd) bs
-- compile n context (C.CoreList els) =
--   case els of
--     [] -> let con = S.standardConstructor 0 stgNil
--           in S.Let (Vector.singleton $ S.PreClosure mempty con)
--              (S.App (fromIntegral n + 1) (S.Ref $ S.Stack 0) mempty)
--     x:xs -> let (refs, k, pcs) = compileArgs context [x, C.CoreList xs]
--                 con = S.standardConstructor (fromIntegral $ length refs) stgCons
--             in S.Let (Vector.fromList $ pcs ++ [S.PreClosure (Vector.fromList refs) con])
--                      (S.App (fromIntegral n + fromIntegral k + 1) (S.Ref $ S.Stack $ fromIntegral k) mempty)
