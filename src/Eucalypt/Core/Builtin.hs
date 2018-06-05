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

import Control.Monad ((>=>), filterM )
import qualified Data.HashMap.Strict.InsOrd as OM
import Data.List (intercalate)
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Text.Regex.PCRE
  ( AllMatches
  , AllTextMatches
  , AllTextSubmatches
  , MatchLength
  , MatchOffset
  , (=~)
  , getAllMatches
  , getAllTextMatches
  , getAllTextSubmatches
  )

type Builtin = WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr

-- | A panic from user code
--
euPanic :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euPanic _ [e@(CorePrim (CoreString s))] = throwEvalError $ Panic s e
euPanic _ as = throwEvalError $ Bug "Bad arguments for panic" (CoreList as)

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
--
-- TODO: metadata transparency!
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
euEq _ as = throwEvalError $ Bug "__EQ called with bad as" (CoreList as)

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
euIf _ as = throwEvalError $ Bug "__IF called with bad as" (CoreList as)

-- | __NOT(b) - 'b' must be boolean. Arity 1. Strict in 'b'. Runtime
-- error if 'b' is not a boolean.
--
euNot :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euNot whnfM [e] = CorePrim . CoreBoolean . not <$> evalBoolean whnfM e
euNot _ as = throwEvalError $ Bug "__NOT called with bad as" (CoreList as)

-- | __AND(l,r) - 'l' and 'r' must be boolean. Arity 2. Strict in 'l'
-- and 'r'.
--
euAnd :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euAnd whnfM [l, r] = do
  lbool <- evalBoolean whnfM l
  rbool <- evalBoolean whnfM r
  return $ CorePrim (CoreBoolean (lbool && rbool))
euAnd _ as = throwEvalError $ Bug "__AND called with bad as" (CoreList as)

-- | __OR(l,r) - 'l' and 'r' must be boolean. Arity 2. Strict in 'l'
-- and 'r'.
--
euOr :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euOr whnfM [l, r] = do
  lbool <- evalBoolean whnfM l
  rbool <- evalBoolean whnfM r
  return $ CorePrim (CoreBoolean (lbool || rbool))
euOr _ as = throwEvalError $ Bug "__OR called with bad as" (CoreList as)

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
arith _ _ as = throwEvalError $ Bug "Arith op called with bad as" (CoreList as)


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

euDiv _ as = throwEvalError $ Bug "Division with bad as" (CoreList as)

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
arithComp _ _ as = throwEvalError $ Bug "Comparison op called with bad as" (CoreList as)

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
euHead _ as = throwEvalError $ Bug "__HEAD called with bad as" (CoreList as)

-- | __TAIL(l) - 'l' must be list. Arity 1. Strict in 'l'.
--
euTail :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euTail whnfM [l] =
  whnfM l >>= \case
    (CoreList (_:t)) -> return (CoreList t)
    (CoreList []) -> throwEvalError $ EmptyList (CoreList [])
    e -> throwEvalError $ NotList e
euTail _ as = throwEvalError $ Bug "__TAIL called with bad as" (CoreList as)

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
euCons _ as = throwEvalError $ Bug "__TAIL called with bad as" (CoreList as)


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


-- | __SYM(s) - create symbol from string s. Strict in s.
--
euSym :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euSym whnfM [s] = whnfM s >>= \case
  (CorePrim (CoreString v)) -> return (CorePrim (CoreSymbol v))
  e -> throwEvalError $ SymbolNamesMustBeStrings e
euSym _ as = throwEvalError $ Bug "__SYM called with bad as" (CoreList as)


-- | __BLOCK(l) builtin. Arity 1. Non-strict. Wrap up a list of elements as a block.
--
euBlock :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euBlock _ [l] = return $ CoreBlock l
euBlock _ as = throwEvalError $ Bug "__BLOCK called with bad as" (CoreList as)



-- | __ELEMENTS(b) builtin. Arity 1. Strict in b. Unwrap a block to expose the elements.
--
euElements :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euElements whnfM [e] =
  whnfM e >>= \case
    CoreBlock l -> return l
    _ -> throwEvalError $ ElementsArgumentNotBlock e
euElements _ as = throwEvalError $ Bug "__ELEMENTS called with bad as" (CoreList as)



-- | Merge together the block elements, maintaining original block
-- ordering where possible but accepting overriddewn values from the
-- latter block. Preserves metadata of the overriding element.
mergeElements :: WhnfEvaluator -> [CoreExpr] -> [CoreExpr] -> Interpreter [CoreExpr]
mergeElements  whnfM l r =
  map listify . OM.toList . fromPairs <$> pairs
  where pairs = mapM (whnfM >=> pair) (l ++ r)
        pair (CoreList [CorePrim (CoreSymbol k), v]) = return (k, (Nothing, v))
        pair (CoreMeta m (CoreList [CorePrim (CoreSymbol k), v])) = return (k, (Just m, v))
        pair el = throwEvalError $ BadBlockElement el
        listify (k, (Just m, v)) = CoreMeta m (CoreList [CorePrim (CoreSymbol k), v])
        listify (k, (Nothing, v)) = CoreList [CorePrim (CoreSymbol k), v]
        fromPairs = foldl (\m (k, v) -> OM.insertWith const k v m) OM.empty



-- | Block merge. The default action for block catentation (or
-- applying a block as a function). Also available as `merge`.
--
euMerge :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euMerge whnfM [l, r] = do
  l' <- whnfM l
  r' <- whnfM r
  case (l', r') of
    (CoreBlock l'', CoreBlock r'') -> do
      l''' <- whnfM l''
      r''' <- whnfM r''
      case (l''', r''') of
        (CoreList ll, CoreList rr) -> CoreBlock . CoreList <$> mergeElements whnfM ll rr
        _ -> throwEvalError $ BadBlockMerge (CoreList [l, r])
    _ -> throwEvalError $ BadBlockMerge (CoreList [l, r])
euMerge _ as = throwEvalError $ BadBlockMerge (CoreList as)



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
buildSearchList whnfM (CoreBlock list) =
  whnfM list >>= skipMeta whnfM >>= \case
    (CoreList items) -> concatResults $ map eval (reverse items)
    e -> throwEvalError $ LookupTargetNotList e
  where
    eval item = whnfM item >>= skipMeta whnfM >>= unwrapBlockElement whnfM
buildSearchList _ e = throwEvalError $ LookupTargetNotList e


-- | Lookup in a block returning Maybe of result.
lookupMaybe :: WhnfEvaluator -> CoreExpr -> CoreRelativeName -> Interpreter (Maybe CoreExpr)
lookupMaybe whnfM e name = do
  obj <- whnfM e
  alist <- buildSearchList whnfM obj
  return $ lookup name alist


-- | Lookup in a block requires realising all keys as later keys
-- override earlier. Return default if key does not exist.
--
lookupOr ::
     WhnfEvaluator -> Interpreter CoreExpr -> CoreExpr -> CoreRelativeName -> Interpreter CoreExpr
lookupOr whnfM d e name = do
  obj <- whnfM e >>= skipMeta whnfM
  alist <- buildSearchList whnfM obj
  case lookup name alist of
    Just val -> return val
    Nothing -> d

-- | __LOOKUPOR(n, d, b) - look up name `n` (string or symbol) in
-- block `b`, returning `d` if it isn't there. Strict in `b` and `r`.
--
euLookupOr ::
     WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euLookupOr whnfM [n, d, b] = do
  b' <- whnfM b
  n' <- whnfM n
  case n' of
    (CorePrim (CoreSymbol s)) -> lookupOr whnfM (return d) b' s
    (CorePrim (CoreString s)) -> lookupOr whnfM (return d) b' s
    _ -> throwEvalError $ LookupKeyNotStringLike n'
euLookupOr _ as = throwEvalError $ Bug "__LOOKUP called with bad arguments" (CoreList as)

-- | __LOOKUP(n, d, b) - look up name `n` (string or symbol) in
-- block `b`, returning `d` if it isn't there. Strict in `b` and `r`.
--
euLookup ::
     WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euLookup whnfM [n, b] = do
  b' <- whnfM b
  n' <- whnfM n
  case n' of
    (CorePrim (CoreSymbol s)) -> lookupName whnfM b' s
    (CorePrim (CoreString s)) -> lookupName whnfM b' s
    _ -> throwEvalError $ LookupKeyNotStringLike n'
euLookup _ as = throwEvalError $ Bug "__LOOKUP called with bad arguments" (CoreList as)

-- | Remove item from block with the specified key
removeItem :: WhnfEvaluator -> CoreExpr -> CoreRelativeName -> Interpreter CoreExpr
removeItem whnfM b name = do
  b' <- whnfM b
  case b' of
    (CoreBlock l) -> do
      items <- whnfM l
      case items of
        (CoreList els) -> CoreBlock . CoreList <$> filterM mismatch els
        _ -> throwEvalError $ BadBlockContent l
    _ -> throwEvalError $ RemoveArgumentNotBlock b
  where mismatch item = whnfM item >>= \case
          (CoreList (CorePrim (CoreSymbol k):_)) -> return $ k == name
          (CoreList (CorePrim (CoreString k):_)) -> return $ k == name
          _ -> throwEvalError $ LookupKeyNotStringLike item

-- | __REMOVE(k, b) remove item with key @k@ from block @b@
euRemove ::
     WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euRemove whnfM [n, b] = do
  b' <- whnfM b
  n' <- whnfM n
  case n' of
    (CorePrim (CoreSymbol s)) -> removeItem whnfM b' s
    (CorePrim (CoreString s)) -> removeItem whnfM b' s
    _ -> throwEvalError $ LookupKeyNotStringLike n'
euRemove _ as = throwEvalError $ Bug "__LOOKUP called with bad arguments" (CoreList as)

-- | Lookup in a block, throwing if key absent.
--
lookupName ::
     WhnfEvaluator -> CoreExpr -> CoreRelativeName -> Interpreter CoreExpr
lookupName whnfM e n = lookupOr whnfM (throwEvalError $ KeyNotFound n) e n

-- | Tedious and shonky... could get this from regex-compat instead
splitRegex :: String -> String -> [String]
splitRegex s re =
  let matches = getAllMatches (s =~ re :: AllMatches [] (MatchOffset, MatchLength))
      spans = map (\(o, l) -> (o, o + l)) matches
      spans' = [(0,0)] ++ spans ++ [(length s, 0)]
      misses = zip (map snd spans') (map fst (tail spans'))
      segments = map (\(b, e) -> take (e - b) (drop b s)) misses
  in filter (not . null) segments

-- | __SPLIT(s, re) - split s on re, string in both.
--
euSplit :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euSplit whnfM [s, re] = do
  s' <- whnfM s
  re' <- whnfM re
  case (s', re') of
    (CorePrim (CoreString target), CorePrim (CoreString regex)) ->
      return $ CoreList $ map (CorePrim . CoreString) $ splitRegex target regex
    _ -> throwEvalError $ BadSplitArgs s re
euSplit _ as = throwEvalError $ Bug "__SPLIT called with bad arguments" (CoreList as)

-- | __JOIN(l, sep) - join (string) items of l with sep.
--
euJoin :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euJoin whnfM [l, sep] = do
  l' <- whnfM l
  sep' <- whnfM sep
  case (l', sep') of
    (CoreList xs, CorePrim (CoreString s)) ->
      CorePrim . CoreString . intercalate s <$> mapM stringItem xs
    _ -> throwEvalError $ BadJoinArgs l' sep'
  where
    stringItem x = whnfM x >>= extractString
    extractString (CorePrim (CoreString x)) = return x
    extractString x = throwEvalError $ NotString x
euJoin _ as = throwEvalError $ Bug "__JOIN called with bad arguments" (CoreList as)

-- | __MATCH(s, re) - match s with re, returning list of full match t
-- index 0 then groups.
--
euMatch :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euMatch whnfM [s, re] = do
  s' <- whnfM s
  re' <- whnfM re
  case (s', re') of
    (CorePrim (CoreString target), CorePrim (CoreString regex)) ->
      return . CoreList . map (CorePrim . CoreString) $
      getAllTextSubmatches (target =~ regex :: AllTextSubmatches [] String)
    _ -> throwEvalError $ BadMatchArgs s re
euMatch _ as = throwEvalError $ Bug "__MATCH called with bad arguments" (CoreList as)

-- | __MATCHES(s, re) - find all matches of @re@ in @s@, returning list of
-- string matches.
--
euMatches :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euMatches whnfM [s, re] = do
  s' <- whnfM s
  re' <- whnfM re
  case (s', re') of
    (CorePrim (CoreString target), CorePrim (CoreString regex)) ->
      return . CoreList . map (CorePrim . CoreString) $
      getAllTextMatches (target =~ regex :: AllTextMatches [] String)
    _ -> throwEvalError $ BadMatchArgs s re
euMatches _ as = throwEvalError $ Bug "__MATCHES called with bad arguments" (CoreList as)

-- | __WITHMETA(m, e) - tag metadata `m` onto expression `e`.
-- Lazy in both as.
--
euWithMeta :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euWithMeta _ [m, e] = return $ CoreMeta m e
euWithMeta _ as =
  throwEvalError $ Bug "__WITHMETA called with bad arguments" (CoreList as)

-- | __META(e) - retrieve metadata from value - which must be a
-- metadata annotated value (not the value contained
-- within).
euMeta :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euMeta whnfM [e] = do
  e' <- whnfM e
  case e' of
    (CoreMeta m _) -> return m
    _ -> return $ block []
euMeta _ as =
  throwEvalError $ Bug "__WITHMETA called with bad arguments" (CoreList as)


-- | __STR(e) - convert to string.
euStr :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euStr whnfM [e] =  whnfM e >>= skipMeta whnfM >>= \case
  (CorePrim (CoreInt n)) -> (return . CorePrim . CoreString . show) n
  (CorePrim (CoreFloat f)) -> (return . CorePrim . CoreString . show) f
  s@(CorePrim (CoreString _)) -> return s
  (CorePrim (CoreSymbol s)) -> (return . CorePrim . CoreString) s
  (CorePrim (CoreBoolean b)) -> (return . CorePrim . CoreString . show) b
  x -> (return . CorePrim . CoreString . show) x
euStr _ as =
  throwEvalError $ Bug "__STR called with bad arguments" (CoreList as)


-- | __CAT(a, b) - the default implementation of catenation
--
euCat :: WhnfEvaluator -> [CoreExpr] -> Interpreter CoreExpr
euCat whnfM [a, b] = (`app` [a]) <$> whnfM b
euCat _ as = throwEvalError $ Bug "__CAT called with bad arguments" (CoreList as)



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
  , ("SYM", (1, euSym))
  , ("BLOCK", (1, euBlock))
  , ("ELEMENTS", (1, euElements))
  , ("MERGE", (2, euMerge))
  , ("LOOKUP", (2, euLookup))
  , ("LOOKUPOR", (3, euLookupOr))
  , ("REMOVE", (2, euRemove))
  , ("SPLIT", (2, euSplit))
  , ("JOIN", (2, euJoin))
  , ("MATCH", (2, euMatch))
  , ("MATCHES", (2, euMatches))
  , ("WITHMETA", (2, euWithMeta))
  , ("META", (1, euMeta))
  , ("STR", (1, euStr))
  , ("CAT", (2, euCat))
  ]

-- | Look up a built in by name, returns tuple of arity and implementation.
--
lookupBuiltin :: CoreBuiltinName -> Maybe (Int, Builtin)
lookupBuiltin n = lookup n builtinIndex
