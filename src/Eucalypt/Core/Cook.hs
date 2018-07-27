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
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Safe (headMay)



-- | Distribute fixities to call sites
--
-- for each let binding (*) = (CoreOperator fixity prec body)
--
-- subtitute (CoreOperator fixity prev (var "(*)")) and alter the
-- binding to (*) = (Lambda body)
--
distributeFixities :: CoreExp a -> CoreExp a
distributeFixities (CoreLet bs b) = CoreLet prunedBindings newBody
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
        (CoreOperator f p _) -> CoreOperator f p
        _ -> id
    exposeCallable (CoreOperator _ _ lambda) = lambda
    exposeCallable e = e
    distributeScopeFixities = Scope . distributeFixities . unscope
distributeFixities (CoreMeta m e) = CoreMeta m $ distributeFixities e
distributeFixities (CoreChecked f e) = CoreChecked f $ distributeFixities e
distributeFixities (CoreTraced e) = CoreTraced $ distributeFixities e
distributeFixities e = e

-- | A core pass prior to evaluation to cook all soup that can be
-- cooked.
cookAllSoup :: Anaphora a => CoreExp a -> Interpreter (CoreExp a)
cookAllSoup = Interpreter . cookBottomUp False

-- | Entrypoint for evaluator if soup is discovered at runtime
cook :: Anaphora a => [CoreExp a] -> Interpreter (CoreExp a)
cook es = case cookSoup False es of
  Right expr -> return expr
  Left err -> throwEvalError err

-- | Take sequence of expression in operator soup and rearrange into
-- non-soup expression using operator fixity.
cookSoup :: Anaphora a => Bool -> [CoreExp a] -> Either EvaluationError (CoreExp a)
cookSoup parentAnaphoric es = do
  subcooked <- cookSubsoups inAnaphoricLambda filled
  expr <- evalState shunt (initState subcooked inAnaphoricLambda)
  if wrap
    then return $ (bindAnaphora . numberAnaphora) expr
    else return expr
  where
    (filled, imAnaphoric) = precook es
    wrap = imAnaphoric && not parentAnaphoric
    inAnaphoricLambda = parentAnaphoric || imAnaphoric

cookScope ::
     (Anaphora a, Eq b, Show b)
  => Bool
  -> Scope b CoreExp a
  -> Interpreter (Scope b CoreExp a)
cookScope anaphoric scope =
  Interpreter $ toScope <$> (cookBottomUp anaphoric . fromScope) scope

-- | Precook...
--
-- Go through each filling first so and determine whether there are
-- anaphora at this level
precook :: Anaphora a => [CoreExp a] -> ([CoreExp a], Bool)
precook = fillGaps



-- | Recurse down cooking any operator soups from the bottom upwards
cookSubsoups :: Anaphora a => Bool -> [CoreExp a] -> Either EvaluationError [CoreExp a]
cookSubsoups anaphoric = mapM (cookBottomUp anaphoric)


cookBottomUp ::
     Anaphora a => Bool -> CoreExp a -> Either EvaluationError (CoreExp a)
cookBottomUp anaphoric (CoreOpSoup exprs) = cookSoup anaphoric exprs
cookBottomUp anaphoric (CoreArgTuple exprs) =
  CoreArgTuple <$> traverse (cookBottomUp anaphoric) exprs
cookBottomUp anaphoric (CoreList exprs) =
  CoreList <$> traverse (cookBottomUp anaphoric) exprs
cookBottomUp anaphoric (CoreBlock l) = CoreBlock <$> cookBottomUp anaphoric l
cookBottomUp anaphoric (CoreMeta m e) = CoreMeta m <$> cookBottomUp anaphoric e
cookBottomUp anaphoric (CoreApply f exprs) =
  CoreApply <$> cookBottomUp anaphoric f <*>
  traverse (cookBottomUp anaphoric) exprs
cookBottomUp anaphoric (CoreLambda n body) =
  CoreLambda n <$> runInterpreter (cookScope anaphoric body)
cookBottomUp anaphoric (CoreLet bs body) = CoreLet <$> newBindings <*> newBody
  where
    newBody = runInterpreter (cookScope anaphoric body)
    newBindings =
      zip (map fst bs) <$>
      traverse ((runInterpreter . cookScope anaphoric) . snd) bs
cookBottomUp _ e = Right e


-- | Run the shunting algorithm until finished or errored
shunt :: Anaphora a => State (ShuntState a) (Either EvaluationError (CoreExp a))
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
  , shuntError :: Maybe EvaluationError
  , shuntInsideAnaphoricLambda :: Bool
  } deriving (Show)

initState :: Anaphora a => [CoreExp a] -> Bool -> ShuntState a
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
setError :: EvaluationError -> State (ShuntState a) ()
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
formApply (CoreBuiltin "*CALL*") [f, CoreArgTuple as] =
  CoreApply f as
formApply (CoreBuiltin "*DOT*") [o, CoreName n] = CoreLookup o n
formApply f as = CoreApply f as

-- | Apply the given operator to the argument(s) at the top of the
-- output stack
applyOp :: Show a => CoreExp a -> State (ShuntState a) ()
applyOp op@(CoreOperator x _ e) =
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
seatOp op@(CoreOperator x p _) =
  peekOp >>= \case
    Just op'@(CoreOperator x' p' _)
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
ensureValidSequence :: Anaphora a => Maybe (CoreExp a) -> State (ShuntState a) ()
ensureValidSequence lhs =
  peekSource >>= \rhs ->
  case validExprSeq lhs rhs of
    Just e@(CoreVar _) -> pushback e -- push an anaphoric var to input
    Just e -> pushback e
    Nothing -> return ()

-- | A step of the shunting yard algorithm
shunt1 :: Anaphora a => State (ShuntState a) ()
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
bindSides (Just (CoreOperator InfixLeft _ _)) = (OpLike, OpLike)
bindSides (Just (CoreOperator InfixRight _ _)) = (OpLike, OpLike)
bindSides (Just (CoreOperator UnaryPostfix _ _)) = (OpLike, ValueLike)
bindSides (Just (CoreOperator UnaryPrefix _ _)) = (ValueLike, OpLike)
bindSides (Just _) = (ValueLike, ValueLike)
bindSides Nothing = (OpLike, OpLike)

-- | Two exprs are valid together if one is OpLike and one is
-- ValueLike on the sides that touch
validExprSeq :: Anaphora a => Maybe (CoreExp a) -> Maybe (CoreExp a) -> Maybe (CoreExp a)
validExprSeq l r = filler ((snd . bindSides) l) ((fst . bindSides) r)

-- | We can make an invalid sequence valid by inserting a catenation
-- op or an anaphoric parameter
filler :: Anaphora a => BindSide -> BindSide -> Maybe (CoreExp a)
filler ValueLike ValueLike = Just catOp
filler OpLike OpLike = Just $ return unnumberedAnaphor
filler _ _ = Nothing

-- | Make a given expression valid by inserting catenation and
-- anaphora as required and track whether the sequence contains any
-- anaphora.
fillGaps :: Anaphora a => [CoreExp a] -> ([CoreExp a], Bool)
fillGaps exprs =
  let (es, anaphoric) = foldl' accum ([], False) sides
  in (reverse es, anaphoric)
  where
    sides = map Just exprs ++ [Nothing]
    anaphoricMaybe = getAny . foldMap Any . fmap isAnaphoricVar
    accum (l, b) e =
      let (l', b') = case validExprSeq (headMay l) e of
                       Just v
                         | isAnaphoricVar v -> (v : l, True)
                         | otherwise -> (v : l, b || anaphoricMaybe e)
                       Nothing -> (l, b || anaphoricMaybe e)
      in case e of
        Just x -> (x: l', b')
        Nothing -> (l', b')
