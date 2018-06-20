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

-- import Debug.Trace
import Control.Monad.Loops (untilM_)
import Control.Monad.State.Lazy
import Eucalypt.Core.Error
import Eucalypt.Core.Interpreter
import Eucalypt.Core.Syn
import Safe (headMay)


-- | Once all operator names have been resolved to 'CoreOperator's we
-- know fixity and precedence so can restructure and identify
-- catenations amongst them.
cook :: [CoreExpr] -> Interpreter CoreExpr
cook es = case cookSoup es of
  Right expr -> return expr
  Left err -> throwEvalError err


-- | Cook expressions recursively but don't go under binders or into
-- metadata.
cookAll :: CoreExpr -> Either EvaluationError CoreExpr
cookAll (CoreOpSoup exprs) = cookSoup exprs
cookAll (CoreArgTuple exprs) = CoreArgTuple <$> traverse cookAll exprs
cookAll (CoreList exprs) = CoreList <$> traverse cookAll exprs
cookAll (CoreBlock l) = CoreBlock <$> cookAll l
cookAll (CoreMeta m e) = CoreMeta m <$> cookAll e
cookAll (CoreApply f exprs) = CoreApply f <$> traverse cookAll exprs
cookAll e = Right e

cookSoup :: [CoreExpr] -> Either EvaluationError CoreExpr
cookSoup = evalState shunt . initState


-- | Run the shunting algorithm until finished or errored
shunt :: State ShuntState (Either EvaluationError CoreExpr)
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
            es -> return $ Left $ InvalidOperatorOutputStack es


-- | State of the shunting yard algorithm
data ShuntState = ShuntState
  { shuntOutput :: [CoreExpr]
  , shuntOps :: [CoreExpr]
  , shuntSource :: [CoreExpr]
  , shuntError :: Maybe EvaluationError
  } deriving (Show)

initState :: [CoreExpr] -> ShuntState
initState es =
  ShuntState
    {shuntOutput = [], shuntOps = [], shuntSource = es, shuntError = Nothing}

complete :: ShuntState -> Bool
complete ShuntState {shuntOps = ops, shuntSource = source} =
  null ops && null source

-- | Set the error state
setError :: EvaluationError -> State ShuntState ()
setError e = modify $ \s -> s {shuntError = Just e}

-- | Pop the next expression from the input soup
popNext :: State ShuntState (Maybe CoreExpr)
popNext =
  state $ \s ->
    case shuntSource s of
      e:es -> (Just e, s {shuntSource = es})
      [] -> (Nothing, s)

-- | Pop next but cook any subsoup in the process
popNextRecur :: State ShuntState (Maybe CoreExpr)
popNextRecur = do
  next <- popNext
  case next of
    Just expr ->
      either
        (\e -> setError e >> return Nothing)
        (return . Just)
        (cookAll expr)
    Nothing -> return Nothing


-- | Return the operator at the top of the operator stack
peekOp :: State ShuntState (Maybe CoreExpr)
peekOp = headMay . shuntOps <$> get

-- | Return the next expression
peekSource :: State ShuntState (Maybe CoreExpr)
peekSource = headMay . shuntSource <$> get

-- | Pop two expressions off the output stack
popTwo :: State ShuntState (Maybe (CoreExpr, CoreExpr))
popTwo =
  state $ \s ->
    case shuntOutput s of
      r:l:es -> (Just (l, r), s {shuntOutput = es})
      _ -> (Nothing, s)

-- | Pop one expression of the output stack
popOne :: State ShuntState (Maybe CoreExpr)
popOne =
  state $ \s ->
    case shuntOutput s of
      e:es -> (Just e, s {shuntOutput = es})
      _ -> (Nothing, s)


-- | Lookups and calls desugar as operators but need special handling
-- so once precedence is resolved we'll transform the syntax. This
-- happens when we apply to the output stack.
formApply :: CoreExpr -> [CoreExpr] -> CoreExpr
formApply (CoreBuiltin "*CALL*") [f, CoreArgTuple as] =
  CoreApply f as
formApply (CoreBuiltin "*DOT*") [o, CoreName n] = CoreLookup o n
formApply f as = CoreApply f as

-- | Apply the given operator to the argument(s) at the top of the
-- output stack
applyOp :: CoreExpr -> State ShuntState ()
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
        Nothing -> setError (TooFewOperands op)
    applyOne =
      popOne >>= \case
        Just l -> pushOutput (formApply e [l])
        Nothing -> setError (TooFewOperands op)
applyOp e = setError $ Bug "applyOp called on non-operator" e

-- | Discard the operator on the top of the operator stack
dropOp :: State ShuntState ()
dropOp =
  modify $ \s ->
    case shuntOps s of
      _:os -> s {shuntOps = os}
      _ -> s

-- | Push supplied operator onto the operator stack
pushOp :: CoreExpr -> State ShuntState ()
pushOp e =
  modify $ \s ->
    case shuntOps s of
      ops -> s {shuntOps = e : ops}

-- | Push operator onto stack, making way by applying any operators of
-- a lower precedence first
seatOp :: CoreExpr -> State ShuntState ()
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
seatOp e = setError $ Bug "seatOp called with non-operator" e

-- | Apply all operators remaining on the operator stack to the output
clearOps :: State ShuntState ()
clearOps =
  peekOp >>= \case
    Just op -> dropOp >> applyOp op >> clearOps
    Nothing -> return ()

-- | Push an expression onto the output stack.
pushOutput :: CoreExpr -> State ShuntState ()
pushOutput e = state $ \s -> ((), s {shuntOutput = e : shuntOutput s})

-- | Push an item onto the input stack
pushback :: CoreExpr -> State ShuntState ()
pushback e = state $ \s -> ((), s {shuntSource = e : shuntSource s})

-- | Check the expression can be safely followed by what's coming up
-- in next in the source and insert catenation operator or implicit
-- parameter otherwise (giving us haskell style "sections" @(+)@ etc.)
ensureValidSequence :: CoreExpr -> State ShuntState ()
ensureValidSequence lhs =
  peekSource >>= \rhs ->
  case validExprSeq (Just lhs) rhs of
    -- TODO: leave sections for separate enhancement
    Just e@(CoreVar _) -> setError $ InvalidOperatorSequence lhs e
    Just e -> pushback e
    Nothing -> return ()

-- | A step of the shunting yard algorithm
shunt1 :: State ShuntState ()
shunt1 =
  popNextRecur >>= \case
    Just expr@CoreOperator {} -> ensureValidSequence expr >> seatOp expr
    Just expr -> ensureValidSequence expr >> pushOutput expr
    Nothing -> clearOps

-- ? operator affinities
--
-- Determine whether sequences of

data BindSide = OpLike | ValueLike
  deriving Eq

-- | Classify each 'CoreExpr' with respect to what can appear on
-- either side
bindSides :: Maybe CoreExpr -> (BindSide, BindSide)
bindSides (Just (CoreOperator InfixLeft _ _)) = (OpLike, OpLike)
bindSides (Just (CoreOperator InfixRight _ _)) = (OpLike, OpLike)
bindSides (Just (CoreOperator UnaryPostfix _ _)) = (OpLike, ValueLike)
bindSides (Just (CoreOperator UnaryPrefix _ _)) = (ValueLike, OpLike)
bindSides (Just _) = (ValueLike, ValueLike)
bindSides Nothing = (OpLike, OpLike)

-- | Two exprs are valid together if one is OpLike and one is
-- ValueLike on the sides that touch
validExprSeq :: Maybe CoreExpr -> Maybe CoreExpr -> Maybe CoreExpr
validExprSeq l r = filler ((snd . bindSides) l) ((fst . bindSides) r)

-- | We can make an invalid sequence valid by inserting a catenation
-- op or an anaphoric parameter
filler :: BindSide -> BindSide -> Maybe CoreExpr
filler ValueLike ValueLike = Just catOp
filler OpLike OpLike = Just $ var "_"
filler _ _ = Nothing
