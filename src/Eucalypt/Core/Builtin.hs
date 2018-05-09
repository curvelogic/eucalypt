{-# LANGUAGE LambdaCase, RankNTypes #-}
{-|
Module      : Eucalypt.Core.Builtin
Description : Implementations of built in functions
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Builtin where

import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn

type Builtin = WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr

-- | A panic from user code
--
euPanic :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euPanic _ [e@(CorePrim (CoreString s))] = throwEvalError $ Panic s e
euPanic _ args = throwEvalError $ Bug "Bad arguments for panic" (CoreList args)

-- | __NULL builtin - evaluates to null primitive. Arity 0.
--
euNull :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euNull _ _ = return $ CorePrim CoreNull

-- | __FALSE builtin - evaluates to false boolean primitive. Arity 0.
--
euFalse :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euFalse _ _ = return $ CorePrim $ CoreBoolean False

-- | __TRUE builtin - evaluates to false boolean primitive. Arity 0.
--
euTrue :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euTrue _ _ = return $ CorePrim $ CoreBoolean True

-- | For comparisons of data structures, if we want to rely on core
-- syntaxes Eq implementation for equality, we need to evaluate a bit
-- more deeply than whnf, otherwise @[1,2,3] !=
-- [head([1,2,3]),tail([1,2,3])]@
forceDataStructures :: WhnfEvaluator -> CoreExpr -> Interpreter CoreExpr
forceDataStructures w (CoreList l) = CoreList <$> mapM (forceDataStructures w) l
forceDataStructures w (CoreBlock e) = CoreBlock <$> forceDataStructures w e
forceDataStructures w e = w e >>= \expr -> case expr of
  (CoreList _) -> forceDataStructures w expr
  (CoreBlock _) -> forceDataStructures w expr
  _ -> return expr

-- | __EQ(l,r) - true iff l = r. Arity 2. Strict in both arguments.
--
euEq :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euEq whnfM [l, r] = do
  l' <- forceDataStructures whnfM l
  r' <- forceDataStructures whnfM r
  return $ (CorePrim . CoreBoolean) (l' == r')
euEq _ args = throwEvalError $ Bug "__EQ called with bad args" (CoreList args)

-- | Evaluate an expression and ensure it's a boolean value.
evalBoolean :: WhnfEvaluator -> CoreExpr -> Interpreter Bool
evalBoolean whnfM expr =
  whnfM expr >>= \case
      (CorePrim (CoreBoolean b)) -> return b
      _ -> throwEvalError $ NotBoolean expr


-- | __IF(c,t,f) - 't' if 'c' is boolean true, 'f' otherwise. Strict
-- in 'c'. Arity 3. Runtime type error if 'c' is not a boolean
euIf :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euIf whnfM [c, t, f] = do
  cbool <- evalBoolean whnfM c
  return $
    if cbool
      then t
      else f
euIf _ args = throwEvalError $ Bug "__IF called with bad args" (CoreList args)

-- | __NOT(b) - 'b' must be boolean. Arity 1. Strict in 'b'. Runtime
-- error if 'b' is not a boolean.
--
euNot :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euNot whnfM [e] = CorePrim . CoreBoolean . not <$> evalBoolean whnfM e
euNot _ args = throwEvalError $ Bug "__NOT called with bad args" (CoreList args)

-- | __AND(l,r) - 'l' and 'r' must be boolean. Arity 2. Strict in 'l'
-- and 'r'.
--
euAnd :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euAnd whnfM [l, r] = do
  lbool <- evalBoolean whnfM l
  rbool <- evalBoolean whnfM r
  return $ CorePrim (CoreBoolean (lbool && rbool))
euAnd _ args = throwEvalError $ Bug "__AND called with bad args" (CoreList args)

-- | __OR(l,r) - 'l' and 'r' must be boolean. Arity 2. Strict in 'l'
-- and 'r'.
--
euOr :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euOr whnfM [l, r] = do
  lbool <- evalBoolean whnfM l
  rbool <- evalBoolean whnfM r
  return $ CorePrim (CoreBoolean (lbool || rbool))
euOr _ args = throwEvalError $ Bug "__OR called with bad args" (CoreList args)

-- | General binary arithmetic implementation, takes care of
-- destructuring and type casting
arith :: WhnfEvaluator -> (forall a. Num a => a -> a -> a) -> [CoreExpr] -> Interpreter CoreExpr
arith whnfM op [l, r] = do
  l' <- whnfM l
  r' <- whnfM r
  case (l', r') of
    (CorePrim (CoreInt i), CorePrim (CoreInt j)) ->
      return $ CorePrim (CoreInt $ i `op` j)
    (CorePrim (CoreFloat i), CorePrim (CoreFloat j)) ->
      return $ CorePrim (CoreFloat $ i `op` j)
    (CorePrim (CoreInt i), CorePrim (CoreFloat j)) ->
      return $ CorePrim (CoreFloat $ fromInteger i `op` j)
    (CorePrim (CoreFloat i), CorePrim (CoreInt j)) ->
      return $ CorePrim (CoreFloat $ i `op` fromInteger j)
    (i, j) -> throwEvalError $ NotNumber (CoreList [i, j])
arith _ _ args = throwEvalError $ Bug "Arith op called with bad args" (CoreList args)


-- | __ADD(l, r) - 'l' and 'r' must be numbers. Arity 2. String in both.
euAdd :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euAdd whnfM = arith whnfM (+)

-- | __SUB(l, r) - 'l' and 'r' must be numbers. Arity 2. String in both.
euSub :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euSub whnfM = arith whnfM (-)

-- | __MUL(l, r) - 'l' and 'r' must be numbers. Arity 2. String in both.
euMul :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euMul whnfM = arith whnfM (*)

-- | __DIV(l, r) - 'l' and 'r' must be numbers. Arity 2. String in
-- both. Panics on divide by zero.
euDiv :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euDiv whnfM [l, r] = do
  l' <- whnfM l
  r' <- whnfM r
  case (l', r') of
    (CorePrim (CoreInt i), CorePrim (CoreInt j)) ->
      check $ fromInteger i / fromInteger j
    (CorePrim (CoreFloat i), CorePrim (CoreFloat j)) -> check $ i / j
    (CorePrim (CoreInt i), CorePrim (CoreFloat j)) -> check $ fromInteger i / j
    (CorePrim (CoreFloat i), CorePrim (CoreInt j)) -> check $ i / fromInteger j
    _ -> throwEvalError $ NotNumber (CoreList [l', r'])
  where
    check val =
      if isInfinite val
        then throwEvalError $ DivideByZero r
        else return $ CorePrim (CoreFloat val)

euDiv _ args = throwEvalError $ Bug "Division with bad args" (CoreList args)

-- | General binary arithmetic comparison implementation, takes care
-- of destructuring and type casting
arithComp :: WhnfEvaluator -> (forall a. Ord a => a -> a -> Bool) -> [CoreExpr] -> Interpreter CoreExpr
arithComp whnfM op [l, r] = do
  l' <- whnfM l
  r' <- whnfM r
  case (l', r') of
    (CorePrim (CoreInt i), CorePrim (CoreInt j)) ->
      return $ CorePrim (CoreBoolean $ i `op` j)
    (CorePrim (CoreFloat i), CorePrim (CoreFloat j)) ->
      return $ CorePrim (CoreBoolean $ i `op` j)
    (CorePrim (CoreInt i), CorePrim (CoreFloat j)) ->
      return $ CorePrim (CoreBoolean $ fromInteger i `op` j)
    (CorePrim (CoreFloat i), CorePrim (CoreInt j)) ->
      return $ CorePrim (CoreBoolean $ i `op` fromInteger j)
    (i, j) -> throwEvalError $ NotNumber (CoreList [i, j])
arithComp _ _ args = throwEvalError $ Bug "Comparison op called with bad args" (CoreList args)

-- | __LT(l, r) - 'l' and 'r' must be numbers. Arity 2. String in both.
euLt :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euLt whnfM = arithComp whnfM (<)

-- | __GT(l, r) - 'l' and 'r' must be numbers. Arity 2. String in both.
euGt :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euGt whnfM = arithComp whnfM (>)

-- | __LTE(l, r) - 'l' and 'r' must be numbers. Arity 2. String in both.
euLte :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euLte whnfM = arithComp whnfM (<=)

-- | __GTE(l, r) - 'l' and 'r' must be numbers. Arity 2. String in both.
euGte :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euGte whnfM = arithComp whnfM (>=)

-- | __HEAD(l) - 'l' must be list. Arity 1. Strict in 'l'.
--
euHead :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euHead whnfM [l] =
  whnfM l >>= \case
    (CoreList (h:_)) -> return h
    (CoreList []) -> throwEvalError $ EmptyList (CoreList [])
    e -> throwEvalError $ NotList e
euHead _ args = throwEvalError $ Bug "__HEAD called with bad args" (CoreList args)

-- | __TAIL(l) - 'l' must be list. Arity 1. Strict in 'l'.
--
euTail :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euTail whnfM [l] =
  whnfM l >>= \case
    (CoreList (_:t)) -> return (CoreList t)
    (CoreList []) -> throwEvalError $ EmptyList (CoreList [])
    e -> throwEvalError $ NotList e
euTail _ args = throwEvalError $ Bug "__TAIL called with bad args" (CoreList args)

-- | __CONS(h, t) - 't' must be list. Arity 2.
--
-- TODO: to be lazier than this we need to expose the list to
-- Eucalypt, i.e. move from native haskell list to cons/nil
-- constructors in Eucalypt space.
euCons :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euCons whnfM [h, t] =
  whnfM t >>= \case
    (CoreList t') -> return (CoreList (h : t'))
    e -> throwEvalError $ NotList e
euCons _ args = throwEvalError $ Bug "__TAIL called with bad args" (CoreList args)


-- | Concatenate two lists or blocks into a list. The default action
-- for list concatenation and also available as `concat`.
--
euConcat :: WhnfEvaluator -> CoreExpr -> CoreExpr -> Interpreter CoreExpr
euConcat whnfM l r = do
  l' <- whnfM l
  r' <- whnfM r
  items <- (++) <$> extract l' <*> extract r'
  return $ CoreList items
  where
    extract :: CoreExpr -> Interpreter [CoreExpr]
    extract e =
      case e of
        CoreList items -> return items
        CoreBlock (CoreList items) -> return items
        _ -> throwEvalError $ ConcatArgumentNotList e

-- | Block merge. The default action for block catentation (or
-- applying a block as a function). Also available as `merge`.
--
euMerge :: WhnfEvaluator -> CoreExpr -> CoreExpr -> Interpreter CoreExpr
euMerge whnfM l r = CoreBlock <$> euConcat whnfM l r



-- | Navigate down through metadata evaluating until we get a
-- non-metadata expression.
skipMeta :: WhnfEvaluator -> CoreExpr -> Interpreter CoreExpr
skipMeta whnfM (CoreMeta _ e) = whnfM e >>= skipMeta whnfM
skipMeta _ e = return e



-- | Unwrap a single block element into name and expr, evaluating the
-- key if necessary to get there.
--
unwrapBlockElement ::
     WhnfEvaluator -> CoreExpr -> Interpreter (String, CoreExpr)
unwrapBlockElement whnfM expr@(CoreList [k, v]) = do
  key <- whnfM k
  case key of
    CorePrim (CoreSymbol n) -> return (n, v)
    _ -> throwEvalError $ BadBlockElement expr
unwrapBlockElement _ expr = throwEvalError $ BadBlockElement expr

-- | From a block, expose an association list for lookup
--
buildSearchList :: WhnfEvaluator -> CoreExpr -> Interpreter [(String, CoreExpr)]
buildSearchList whnfM (CoreBlock (CoreList items)) =
  concatResults $ map eval (reverse items)
  where
    eval item = whnfM item >>= skipMeta whnfM >>= unwrapBlockElement whnfM
buildSearchList _ e = throwEvalError $ LookupTargetNotList e

-- | Lookup in a block requires realising all keys as later keys
-- override earlier. Return default if key does not exist.
--
euLookupOr ::
     WhnfEvaluator -> Interpreter CoreExpr -> CoreExpr -> CoreRelativeName -> Interpreter CoreExpr
euLookupOr whnfM d e name = do
  obj <- whnfM e
  alist <- buildSearchList whnfM obj
  case lookup name alist of
    Just val -> return val
    Nothing -> d

-- | Lookup in a block, throwing if key absent.
--
euLookup ::
     WhnfEvaluator -> CoreExpr -> CoreRelativeName -> Interpreter CoreExpr
euLookup whnfM e n = euLookupOr whnfM (throwEvalError $ KeyNotFound n) e n


-- | The builtins exposed to the language.
--
builtinIndex :: [(CoreBuiltinName, (Int, Builtin))]
builtinIndex =
  [ ("PANIC", (1, euPanic))
  , ("NULL", (0, euNull))
  , ("FALSE", (0, euFalse))
  , ("TRUE", (0, euTrue))
  , ("EQ", (2, euEq))
  , ("IF", (3, euIf))
  , ("NOT", (1, euNot))
  , ("AND", (2, euAnd))
  , ("OR", (2, euOr))
  , ("HEAD", (1, euHead))
  , ("TAIL", (1, euTail))
  , ("CONS", (2, euCons))
  , ("MUL", (2, euMul))
  , ("SUB", (2, euSub))
  , ("ADD", (2, euAdd))
  , ("DIV", (2, euDiv))
  , ("LT", (2, euLt))
  , ("GT", (2, euGt))
  , ("LTE", (2, euLte))
  , ("GTE", (2, euGte))
  ]

-- | Look up a built in by name, returns tuple of arity and implementation.
--
lookupBuiltin :: CoreBuiltinName -> Maybe (Int, Builtin)
lookupBuiltin n = lookup n builtinIndex
