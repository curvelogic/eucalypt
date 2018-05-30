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
cook es = case evalState shunt (initState es) of
  Right expr -> return expr
  Left err -> throwEvalError err

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

-- | Catenation operator
catOp :: CoreExpr
catOp = infixl_ 20 (CoreBuiltin "CAT")

-- | State of the shunting yard algorithm
data ShuntState = ShuntState
  { shuntOutput :: [CoreExpr]
  , shuntOps :: [CoreExpr]
  , shuntSource :: [CoreExpr]
  , shuntError :: Maybe EvaluationError
  }

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

-- | Return the operator at the top of the operator stack
peekOp :: State ShuntState (Maybe CoreExpr)
peekOp = headMay . shuntOps <$> get

-- | Pop two expressions off the output stack
popTwo :: State ShuntState (Maybe (CoreExpr, CoreExpr))
popTwo =
  state $ \s ->
    case shuntOutput s of
      r:l:es -> (Just (l, r), s {shuntOutput = es})
      _ -> (Nothing, s)

-- | Pop one expressino of the output stack
popOne :: State ShuntState (Maybe CoreExpr)
popOne =
  state $ \s ->
    case shuntOutput s of
      e:es -> (Just e, s {shuntOutput = es})
      _ -> (Nothing, s)

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
        Just (l, r) -> pushOutput (CoreApply e [l, r])
        Nothing -> setError (TooFewOperands op)
    applyOne =
      popOne >>= \case
        Just l -> pushOutput (CoreApply e [l])
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
seatOp op@(CoreOperator _ p _) = do
  ops <- gets shuntOps
  case ops of
    o@(CoreOperator x' p' _):os ->
      if (p < p') || (p' == p && (x' == InfixLeft))
        then dropOp >> applyOp o >> seatOp op
        else modify $ \s -> s {shuntOps = op : o : os}
    _ -> pushOp op
seatOp e = setError $ Bug "seatOps called with non-operator" e

-- | Apply all operators remaining on the operator stack to the output
clearOps :: State ShuntState ()
clearOps =
  peekOp >>= \case
    Just op -> dropOp >> applyOp op >> clearOps
    Nothing -> return ()

-- | Push an expression onto the output stack
pushOutput :: CoreExpr -> State ShuntState ()
pushOutput e = state $ \s -> ((), s {shuntOutput = e : shuntOutput s})

-- | True if the operator stack is empty
opsExhausted :: State ShuntState Bool
opsExhausted = null <$> gets shuntOps

-- | True if the output stack is non-empty
outputPending :: State ShuntState Bool
outputPending = not . null <$> gets shuntOutput

-- | A step of the shunting yard algorithm
shunt1 :: State ShuntState ()
shunt1 =
  popNext >>= \case
    Just expr@CoreOperator{} -> seatOp expr
    Just expr -> do
      noOps <- opsExhausted
      outputExists <- outputPending
      when (outputExists && noOps) $ seatOp catOp -- recognise catenations at last
      pushOutput expr
    Nothing -> clearOps
