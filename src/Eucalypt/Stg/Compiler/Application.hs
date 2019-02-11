{-# LANGUAGE FlexibleContexts #-}
{-|
Module      : Eucalypt.Stg.Compiler.CompileCore
Description : Compile core to STG-code
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Compiler.Application where

import Control.Monad.State
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Eucalypt.Core.Syn
import Eucalypt.Stg.Compiler.Common
import Eucalypt.Stg.Compiler.Context
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Syn

-- | Analysis of application argument
data ArgCase a
  = EnvRef Ref
  | Scrutinee (Maybe Int)
              (CoreExp a)
  | Closure (Maybe Int)
            (CoreExp a)
  deriving (Show, Eq)

mapClosureExpr ::
     Applicative f => (CoreExp v -> f (ArgCase v)) -> ArgCase v -> f (ArgCase v)
mapClosureExpr f (Closure _ expr) = f expr
mapClosureExpr _ expr = pure expr

scrutinees :: [ArgCase a] -> [(Int, CoreExp a)]
scrutinees = mapMaybe scrutinee
  where
    scrutinee (Scrutinee (Just i) expr) = Just (i, expr)
    scrutinee _ = Nothing

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

-- | Number the scrutinees (from 0), returning number cases plus next counter
numberScrutinees :: ([ArgCase a], Int) -> ([ArgCase a], Int)
numberScrutinees (cs, next) = first reverse $ foldl acc ([], next) cs
  where
    acc (o, n) i =
      case i of
        (Scrutinee Nothing x) -> (Scrutinee (Just n) x : o, n + 1)
        _ -> (i : o, n)



-- | Number the pre closures (from next counter), returning numbered cases
numberPreClosures :: ([ArgCase a], Int) -> ([ArgCase a], Int)
numberPreClosures (cs, next) = first reverse $ foldl acc ([], next) cs
  where
    acc (o, n) i =
      case i of
        (Closure Nothing x) -> (Closure (Just n) x : o, n + 1)
        _ -> (i : o, n)


-- | Number the argument cases, placing cases outside lets
numberArgCases :: [ArgCase a] -> [ArgCase a]
numberArgCases acs = fst $ numberPreClosures $ numberScrutinees (acs, 0)

-- | Number the argument cases, placing cases outside lets
numberArgCasesLetsOutward :: [ArgCase a] -> [ArgCase a]
numberArgCasesLetsOutward acs =
  fst $ numberScrutinees $ numberPreClosures (acs, 0)

-- | Sort those args which need evaluating or allocating into
-- scrutinees then preclosures in the order dictated by numbering
wrappers :: [ArgCase a] -> ([ArgCase a], [ArgCase a])
wrappers = partitionEithers . map classify . filter notEnvRef
  where
    classify c@Scrutinee{} = Left c
    classify c = Right c
    notEnvRef (EnvRef _) = False
    notEnvRef _ = True


-- | Analyse a CoreApply's components to determine how each component
-- should be compiled.
analyseCallRefs :: Context v -> CoreExp v -> [CoreExp v] -> [ArgCase v]
analyseCallRefs context f xs = foldl acc components0 (zip xs strictness)
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
        (CorePrim _ n) -> [EnvRef $ literal n]
        _ ->
          case strict of
            NonStrict -> [Closure Nothing x]
            Strict -> [Scrutinee Nothing x]
    op fn =
      case fn of
        (CoreOperator _ _x _p e) -> e
        _ -> fn


analyse2 :: CoreExp v -> [CoreExp v] -> [ArgCase v]
analyse2 f xs = foldl acc components0 (zip xs strictness)
  where
    strictness =
      case op f of
        (CoreBuiltin _ n) -> globalSignature n
        _ -> replicate (length xs) NonStrict
    components0 = [Closure Nothing f]
    acc comps (x, strict) =
      comps ++
      case strict of
        NonStrict -> [Closure Nothing x]
        Strict -> [Scrutinee Nothing x]
    op fn =
      case fn of
        (CoreOperator _ _x _p e) -> e
        _ -> fn


-- | Compile the central apply expression
compileCall :: Int -> [ArgCase a] -> StgSyn
compileCall envSz components =
  App (Ref $ toRef envSz $ head components) $
  refs (map (toRef envSz) $ tail components)


-- | Process arg cases to extract the free refs to declare in a
-- preclosure and update the cases to reflect the internal refs
-- to use to reference thm.
sortArgCaseRefs :: [ArgCase a] -> ([Ref], [ArgCase a])
sortArgCaseRefs cs =
  (`evalState` 0) $ first concat . unzip <$> traverse renumber cs
  where
    renumber (EnvRef (L i)) = do
      r <- gets L
      modify (+ 1)
      return ([L i], EnvRef r)
    renumber c = return ([], c)


-- | When we're compiling cases inside a PreClosure, we need to ensure
-- the scrutinees' free vars are represented in the PreClosure's free
-- references. This extracts them.
freeScrutineeRefs :: (a -> Ref) -> [ArgCase a] -> [Ref]
freeScrutineeRefs context cs = nub $ concatMap freeRefs cs
  where
    freeRefs (Scrutinee _ expr) = map context $ toList expr
    freeRefs _ = []
