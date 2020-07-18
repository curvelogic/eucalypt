{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Eucalypt.Core.Cook
Description : Pass for "cooking" operator soup into final syntax tree
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental
-}
module Eucalypt.Core.Cook where

import Bound.Scope
import Control.Monad.Loops (untilM_)
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.List (foldl')
import Data.Monoid
import Eucalypt.Core.Anaphora
import Eucalypt.Core.Error
import Eucalypt.Core.GenLookup (eliminateLookupOp)
import Eucalypt.Core.Syn
import Safe (headMay)



-- | The interpreter monad
newtype Interpreter a = Interpreter { runInterpreter :: Either CoreError a }
  deriving (Show, Functor, Applicative, Monad)


-- | Abort interpreter with 'CoreError'
throwEvalError :: CoreError -> Interpreter a
throwEvalError = Interpreter . Left


-- | Distribute fixities to call sites
--
-- for each let binding (*) = (CoreOperator fixity prec body)
--
-- subtitute (CoreOperator fixity prev (var "(*)")) and alter the
-- binding to (*) = (Lambda body)
--
distributeFixities :: CoreExp a -> CoreExp a
distributeFixities (CoreLet smid bs b _) =
  CoreLet smid prunedBindings newBody OtherLet
  where
    newBody = modifyBoundVars bindSiteReplace $ distributeScopeFixities b
    distBindings =
      map
        (second (modifyBoundVars bindSiteReplace . distributeScopeFixities))
        bs
    prunedBindings =
      map (second $ Scope . exposeCallable . unscope) distBindings
    bindSiteReplace z =
      case unscope $ snd $ bs !! z of
        (CoreOperator smid' f p _) -> CoreOperator smid' f p
        _ -> id
    exposeCallable (CoreOperator _ _ _ lambda) = lambda
    exposeCallable e = e
    distributeScopeFixities = Scope . distributeFixities . unscope
distributeFixities (CoreMeta smid m e) = CoreMeta smid m $ distributeFixities e
distributeFixities e = e

-- | A core pass prior to evaluation to cook all soup that can be
-- cooked.
cookAllSoup ::
     (Anaphora SymbolicAnaphora a) => CoreExp a -> Interpreter (CoreExp a)
cookAllSoup = Interpreter . cookBottomUp False

-- | Entrypoint for evaluator if soup is discovered at runtime
cook :: (Anaphora SymbolicAnaphora a) => [CoreExp a] -> Interpreter (CoreExp a)
cook es = case cookSoup False es of
  Right expr -> return expr
  Left err -> throwEvalError err

-- | Take sequence of expression in operator soup and rearrange into
-- non-soup expression using operator fixity.
cookSoup ::
     (Anaphora SymbolicAnaphora a)
  => Bool
  -> [CoreExp a]
  -> Either CoreError (CoreExp a)
cookSoup parentAnaphoric es = do
  subcooked <- cookSubsoups inAnaphoricLambda filled
  expr <- evalState shunt (initState subcooked inAnaphoricLambda)
  if wrap
    then return $ processAnaphora expr
    else return expr
  where
    (filled, imAnaphoric) = precook es
    wrap = imAnaphoric && not parentAnaphoric
    inAnaphoricLambda = parentAnaphoric || imAnaphoric
    processAnaphora = bindAnaphora expressionAnaphora . numberAnaphora expressionAnaphora

cookScope ::
     (Eq b, Show b, Anaphora SymbolicAnaphora a)
  => Bool
  -> Scope b CoreExp a
  -> Interpreter (Scope b CoreExp a)
cookScope anaphoric scope =
  Interpreter $ toScope <$> (cookBottomUp anaphoric . fromScope) scope

-- | Precook...
--
-- Go through each filling first so and determine whether there are
-- anaphora at this level
precook :: (Anaphora SymbolicAnaphora a) => [CoreExp a] -> ([CoreExp a], Bool)
precook = fillGaps



-- | Recurse down cooking any operator soups from the bottom upwards
cookSubsoups ::
     (Anaphora SymbolicAnaphora a)
  => Bool
  -> [CoreExp a]
  -> Either CoreError [CoreExp a]
cookSubsoups anaphoric = mapM (cookBottomUp anaphoric)


cookBottomUp ::
     (Anaphora SymbolicAnaphora a)
  => Bool
  -> CoreExp a
  -> Either CoreError (CoreExp a)
cookBottomUp anaphoric (CoreOpSoup _ exprs) = cookSoup anaphoric exprs
cookBottomUp anaphoric (CoreArgTuple smid exprs) =
  CoreArgTuple smid <$> traverse (cookBottomUp anaphoric) exprs
cookBottomUp anaphoric (CoreList smid exprs) =
  CoreList smid <$> traverse (cookBottomUp anaphoric) exprs
cookBottomUp anaphoric (CoreBlock smid l) = CoreBlock smid <$> cookBottomUp anaphoric l
cookBottomUp anaphoric (CoreMeta smid m e) = CoreMeta smid m <$> cookBottomUp anaphoric e
cookBottomUp anaphoric (CoreApply smid f exprs) =
  CoreApply smid <$> cookBottomUp anaphoric f <*>
  traverse (cookBottomUp anaphoric) exprs
cookBottomUp anaphoric (CoreLambda smid i n body) =
  CoreLambda smid i n <$> runInterpreter (cookScope anaphoric body)
cookBottomUp anaphoric (CoreLet smid bs body _) =
  CoreLet smid <$> newBindings <*> newBody <*> pure OtherLet
  where
    newBody = runInterpreter (cookScope anaphoric body)
    newBindings =
      zip (map fst bs) <$>
      traverse ((runInterpreter . cookScope anaphoric) . snd) bs
cookBottomUp _ e = Right e


-- | Run the shunting algorithm until finished or errored
shunt ::
     (Anaphora SymbolicAnaphora a)
  => State (ShuntState a) (Either CoreError (CoreExp a))
shunt = (shunt1 `untilM_` finished) >> result
  where
    finished = complete <$> get
    result = do
      output <- gets shuntOutput
      err <- gets shuntError
      case err of
        Just e -> return $ Left e
        Nothing ->
          case output of
            [o] -> return $ Right o
            es -> return $ Left $ InvalidOperatorOutputStack (map CoreExpShow es)


-- | State of the shunting yard algorithm
data ShuntState a = ShuntState
  { shuntOutput :: [CoreExp a]
  , shuntOps :: [CoreExp a]
  , shuntSource :: [CoreExp a]
  , shuntError :: Maybe CoreError
  , shuntInsideAnaphoricLambda :: Bool
  } deriving (Show)

initState :: [CoreExp a] -> Bool -> ShuntState a
initState es anaphoric =
  ShuntState
    { shuntOutput = []
    , shuntOps = []
    , shuntSource = es
    , shuntError = Nothing
    , shuntInsideAnaphoricLambda = anaphoric
    }

complete :: ShuntState a -> Bool
complete ShuntState {shuntOps = ops, shuntSource = source} =
  null ops && null source

-- | Set the error state
setError :: CoreError -> State (ShuntState a) ()
setError e = modify $ \s -> s {shuntError = Just e}

-- | Pop the next expression from the input soup
popNext :: State (ShuntState a) (Maybe (CoreExp a))
popNext =
  state $ \s ->
    case shuntSource s of
      e:es -> (Just e, s {shuntSource = es})
      [] -> (Nothing, s)

-- | Return the operator at the top of the operator stack
peekOp :: State (ShuntState a) (Maybe (CoreExp a))
peekOp = headMay . shuntOps <$> get

-- | Return the next expression
peekSource :: State (ShuntState a) (Maybe (CoreExp a))
peekSource = headMay . shuntSource <$> get

-- | Pop two expressions off the output stack
popTwo :: State (ShuntState a) (Maybe (CoreExp a, CoreExp a))
popTwo =
  state $ \s ->
    case shuntOutput s of
      r:l:es -> (Just (l, r), s {shuntOutput = es})
      _ -> (Nothing, s)

-- | Pop one expression of the output stack
popOne :: State (ShuntState a) (Maybe (CoreExp a))
popOne =
  state $ \s ->
    case shuntOutput s of
      e:es -> (Just e, s {shuntOutput = es})
      _ -> (Nothing, s)


-- | Lookups and calls desugar as operators but need special handling
-- so once precedence is resolved we'll transform the syntax. This
-- happens when we apply to the output stack.
formApply :: CoreExp a -> [CoreExp a] -> CoreExp a
formApply (CoreBuiltin _ "*CALL*") [f, CoreArgTuple smid as] =
  CoreApply smid f as
formApply (CoreBuiltin _ "*DOT*") [o, x] = eliminateLookupOp o x
formApply f as = CoreApply (sourceMapId f) f as

-- | Apply the given operator to the argument(s) at the top of the
-- output stack
applyOp :: Show a => CoreExp a -> State (ShuntState a) ()
applyOp op@(CoreOperator _ x _ e) =
  case x of
    InfixLeft -> applyTwo
    InfixRight -> applyTwo
    UnaryPrefix -> applyOne
    UnaryPostfix -> applyOne
  where
    applyTwo =
      popTwo >>= \case
        Just (l, r) -> pushOutput (formApply e [l, r])
        Nothing -> setError (TooFewOperands $ CoreExpShow op)
    applyOne =
      popOne >>= \case
        Just l -> pushOutput (formApply e [l])
        Nothing -> setError (TooFewOperands $ CoreExpShow op)
applyOp e = setError $ Bug "applyOp called on non-operator" (CoreExpShow e)

-- | Discard the operator on the top of the operator stack
dropOp :: State (ShuntState a) ()
dropOp =
  modify $ \s ->
    case shuntOps s of
      _:os -> s {shuntOps = os}
      _ -> s

-- | Push supplied operator onto the operator stack
pushOp :: CoreExp a -> State (ShuntState a) ()
pushOp e =
  modify $ \s ->
    case shuntOps s of
      ops -> s {shuntOps = e : ops}

-- | Push operator onto stack, making way by applying any operators of
-- a lower precedence first
seatOp :: Show a => CoreExp a -> State (ShuntState a) ()
seatOp op@(CoreOperator _ x p _) =
  peekOp >>= \case
    Just op'@(CoreOperator _ x' p' _)
      | x' == InfixLeft ->
        if p <= p'
          then dealWith op'
          else pushOp op
      | x' == InfixRight ->
        if p < p'
          then dealWith op'
          else pushOp op
      | x' == UnaryPostfix -> dealWith op'
      | x == UnaryPostfix ->
        if p < p'
          then dealWith op'
          else applyOp op -- don't put it on stack
      | x' == UnaryPrefix ->
        if p < p'
          then dealWith op'
          else pushOp op
    _ -> pushOp op
  where
    dealWith o = dropOp >> applyOp o >> seatOp op
seatOp e = setError $ Bug "seatOp called with non-operator" (CoreExpShow e)

-- | Apply all operators remaining on the operator stack to the output
clearOps :: Show a => State (ShuntState a) ()
clearOps =
  peekOp >>= \case
    Just op -> dropOp >> applyOp op >> clearOps
    Nothing -> return ()

-- | Push an expression onto the output stack.
pushOutput :: CoreExp a -> State (ShuntState a) ()
pushOutput e = state $ \s -> ((), s {shuntOutput = e : shuntOutput s})

-- | Push an item onto the input stack
pushback :: CoreExp a -> State (ShuntState a) ()
pushback e = state $ \s -> ((), s {shuntSource = e : shuntSource s})

-- | Check the expression can be safely followed by what's coming up
-- in next in the source and insert catenation operator or implicit
-- parameter otherwise (giving us haskell style "sections" @(+)@ etc.)
ensureValidSequence ::
     (Anaphora SymbolicAnaphora a)
  => Maybe (CoreExp a)
  -> State (ShuntState a) ()
ensureValidSequence lhs =
  peekSource >>= \rhs ->
  case validExprSeq lhs rhs of
    Just e@(CoreVar _ _) -> pushback e -- push an anaphoric var to input
    Just e -> pushback e
    Nothing -> return ()

-- | A step of the shunting yard algorithm
shunt1 :: (Anaphora SymbolicAnaphora a) => State (ShuntState a) ()
shunt1 =
  popNext >>= \case
    Just expr@CoreOperator {} -> ensureValidSequence (Just expr) >> seatOp expr
    Just expr -> ensureValidSequence (Just expr) >> pushOutput expr
    Nothing -> clearOps

-- ? operator affinities
--
-- Determine whether sequences of

data BindSide = OpLike | ValueLike
  deriving Eq

-- | Classify each 'CoreExp a' with respect to what can appear on
-- either side
bindSides :: Maybe (CoreExp a) -> (BindSide, BindSide)
bindSides (Just (CoreOperator _ InfixLeft _ _)) = (OpLike, OpLike)
bindSides (Just (CoreOperator _ InfixRight _ _)) = (OpLike, OpLike)
bindSides (Just (CoreOperator _ UnaryPostfix _ _)) = (OpLike, ValueLike)
bindSides (Just (CoreOperator _ UnaryPrefix _ _)) = (ValueLike, OpLike)
bindSides (Just _) = (ValueLike, ValueLike)
bindSides Nothing = (OpLike, OpLike)

-- | Two exprs are valid together if one is OpLike and one is
-- ValueLike on the sides that touch
validExprSeq ::
     (Anaphora SymbolicAnaphora a)
  => Maybe (CoreExp a)
  -> Maybe (CoreExp a)
  -> Maybe (CoreExp a)
validExprSeq l r = filler ((snd . bindSides) l) ((fst . bindSides) r)

-- | We can make an invalid sequence valid by inserting a catenation
-- op or an anaphoric parameter
filler :: (Anaphora SymbolicAnaphora a) => BindSide -> BindSide -> Maybe (CoreExp a)
filler ValueLike ValueLike = Just catOp
filler OpLike OpLike = Just $ return $ unnumberedAnaphor expressionAnaphora
filler _ _ = Nothing

-- | Make a given expression valid by inserting catenation and
-- anaphora as required and track whether the sequence contains any
-- anaphora.
fillGaps :: (Anaphora SymbolicAnaphora a) => [CoreExp a] -> ([CoreExp a], Bool)
fillGaps exprs =
  let (es, anaphoric) = foldl' accum ([], False) sides
   in (reverse es, anaphoric)
  where
    sides = map Just exprs ++ [Nothing]
    anaphoricMaybe =
      getAny . foldMap (Any . isAnaphoricVar expressionAnaphora)
    accum (l, b) e =
      let (l', b') =
            case validExprSeq (headMay l) e of
              Just v
                | isAnaphoricVar expressionAnaphora v -> (v : l, True)
                | otherwise -> (v : l, b || anaphoricMaybe e)
              Nothing -> (l, b || anaphoricMaybe e)
       in case e of
            Just x -> (x : l', b')
            Nothing -> (l', b')
