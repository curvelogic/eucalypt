{-|
Module      : Eucalypt.Stg.Eval
Description : STG evaluation steps
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}
module Eucalypt.Stg.Eval where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad (zipWithM_)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Eucalypt.Stg.Error
import Eucalypt.Stg.Intrinsics
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Syn
import Prelude hiding (log)

-- | Allocate a PAP to record args provided so far and the function
-- info already discovered
allocPartial :: ValVec -> MachineState -> LambdaForm -> ValVec -> IO Address
allocPartial le _ms lf xs = allocate pap
  where
    pap =
      PartialApplication {papCode = lf, papEnv = le, papArgs = xs, papArity = a}
    a = fromIntegral (_free lf) - envSize xs

-- | Push a continuation onto the stack
push :: MachineState -> Continuation -> MachineState
push ms@MachineState {machineStack = st} v =
  ms {machineStack = Vector.snoc st v}

-- | Pop a continuation off the stack
pop :: MonadThrow m => MachineState -> m (Maybe Continuation, MachineState)
pop ms@MachineState {machineStack = st} =
  if Vector.null st
    then return (Nothing, ms)
    else return (Just $ Vector.last st, ms {machineStack = Vector.init st})

-- | Push an ApplyToArgs continuation on the stack
pushApplyToArgs :: MonadThrow m => MachineState -> ValVec -> m MachineState
pushApplyToArgs ms@MachineState {machineStack = stack} xs =
  return $ ms {machineStack = stack `Vector.snoc` ApplyToArgs xs}

-- | Branch expressions expect to find their args as the top entries
-- in the environment (and right now the compiler needs to work out
-- where...)
selectBranch :: BranchTable -> Tag -> Maybe StgSyn
selectBranch (BranchTable bs _) t = snd <$> Map.lookup t bs

-- | Match a native branch table alternative, return the next
-- expression to eval
selectNativeBranch :: NativeBranchTable -> Native -> Maybe StgSyn
selectNativeBranch (NativeBranchTable bs _) n = HM.lookup n bs

-- | Call the machine's trace function
traceOut :: MachineState -> IO ()
traceOut ms@MachineState {machineTrace = tr} = tr ms

-- | Halt the machine
terminate :: MachineState -> MachineState
terminate ms@MachineState{} = ms {machineTerminated = True}

-- | Call a lambda form by putting args in environment and rewiring
-- refs then evaluating.
call :: MonadThrow m => ValVec -> MachineState -> LambdaForm -> ValVec -> m Code
call env _ms code addrs = do
  let env' = env <> addrs
  let code' = argsAt (fromIntegral (envSize env)) (_body code)
  return (Eval code' env')

-- | Main machine step function
step :: MachineState -> IO MachineState
step ms@MachineState {machineTerminated = True} = do
  traceOut ms
  throwM SteppingTerminated
step ms@MachineState {machineCode = (Eval (App f xs) env)} = do
  traceOut ms
  let len = length xs
  case f of
    Ref r -> do
      addr <- resolveHeapObject env ms r
      obj <- peek addr
      case obj of
        Closure lf@LambdaForm {_bound = ar} le ->
          case compare (fromIntegral len) ar
            -- EXACT
                of
            EQ -> tick . setCode ms <$> (vals env ms xs >>= call le ms lf)
            -- CALLK
            GT ->
              let (enough, over) = Vector.splitAt (fromIntegral ar) xs
               in vals env ms over >>= pushApplyToArgs ms >>= \s ->
                    tick . setCode s <$> (vals env ms enough >>= call le ms lf)
            -- PAP2
            LT ->
              vals env ms xs >>= allocPartial le ms lf >>= \a ->
                return $
                tick $
                setCode ms (Eval (Atom (Local 0)) (singleton (StgAddr a)))
        -- PCALL
        PartialApplication code le args ar ->
          case compare (fromIntegral len) ar of
            EQ ->
              vals env ms xs >>= \as ->
                tick . setCode ms <$> call le ms code (args <> as)
            GT ->
              let (enough, over) = Vector.splitAt (fromIntegral ar) xs
               in vals env ms over >>= pushApplyToArgs ms >>= \s ->
                    vals env ms enough >>= \as ->
                      tick . setCode s <$> call le ms code (args <> as)
            LT ->
              vals env ms xs >>= allocPartial le ms code >>= \a ->
                return $
                tick $
                setCode ms (Eval (Atom (Local 0)) (singleton (StgAddr a)))
        BlackHole -> throwM EnteredBlackHole
    -- must be saturated
    Con t -> do
      env' <- vals env ms xs
      return $ tick $ setCode ms (ReturnCon t env')
    -- must be saturated
    Intrinsic i ->
      let mf = intrinsicFunction i
       in tick <$> (vals env ms xs >>= mf ms)
-- | LET
step ms@MachineState {machineCode = (Eval (Let pcs body) env)} = do
  traceOut ms
  addrs <- traverse (allocClosure env ms) pcs
  let env' = env <> (ValVec . Vector.map StgAddr) addrs
  return $ tick $ setCode ms (Eval body env')
-- | LET (recursive)
step ms@MachineState {machineCode = (Eval (LetRec pcs body) env)} = do
  traceOut ms
  addrs <- sequenceA $ replicate (length pcs) (allocate BlackHole)
  let env' = env <> (toValVec . map StgAddr) addrs
  closures <- traverse (buildClosure env' ms) pcs
  zipWithM_ poke addrs (toList closures)
  return $ tick $ setCode ms (Eval body env')
-- | CASE
step ms@MachineState {machineCode = (Eval (Case syn k) env)} = do
  traceOut ms
  return $ tick $ setCode (push ms (Branch k env)) (Eval syn env)
-- | CASE (lit)
step ms@MachineState {machineCode = (Eval (CaseLit syn k) env)} = do
  traceOut ms
  return $ tick $ setCode (push ms (NativeBranch k env)) (Eval syn env)

-- | ReturnCon - returns a data structure into a BranchTable branch
step ms@MachineState {machineCode = (ReturnCon t xs)} = do
  traceOut ms
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch k le)) ->
      case selectBranch k t of
        -- | CASECON
        (Just expr) -> return $ tick $ setCode ms' (Eval expr (le <> xs))
        -- | CASEANY
        Nothing -> do
          addr <-
            allocate
              (Closure
                 (LambdaForm 0 0 False (App (Con t) (locals 0 (envSize xs))))
                 xs)
          case defaultBranch k of
            (Just expr) ->
              return $
              tick $ setCode ms' (Eval expr (le <> singleton (StgAddr addr)))
            Nothing -> throwM NoBranchFound
    (Just NativeBranch{}) -> throwM NativeBranchTableForCon
    (Just (Update a)) -> do
      poke a (Closure (standardConstructor (envSize xs) t) xs)
      return $ tick ms'
    (Just (ApplyToArgs _)) -> throwM ArgInsteadOfBranchTable
    Nothing -> return $ tick $ terminate ms'

-- | ReturnLit - returns a native value to a NativeBranchTable or
-- terminates if none.
step ms@MachineState {machineCode = (ReturnLit nat)} = do
  traceOut ms
  (entry, ms') <- pop ms
  case entry of
    (Just (NativeBranch k le)) ->
      case selectNativeBranch k nat of
        -- CASECON
        (Just expr) -> return $ tick $ setCode ms' (Eval expr le)
        -- CASEANY (lit)
        Nothing ->
          case defaultBranch k of
            (Just expr) ->
              return $ tick $ setCode ms' (Eval expr (le <> singleton (StgNat nat)))
            Nothing -> throwM NoBranchFound
    (Just (Branch _ _)) -> throwM ConBranchTableForNative
    (Just (Update _)) -> throwM LiteralUpdate
    (Just (ApplyToArgs _)) -> throwM ArgInsteadOfNativeBranchTable
    Nothing -> return $ tick $ terminate ms'

-- | In most cases, we punt on to Eval App which should cause
-- ReturnCon or ReturnLit when we reach a value
step ms@MachineState {machineCode = (Eval (Atom ref) env)} = do
  traceOut ms
  v <- val env ms ref
  case v of
    StgAddr _ -> do
      (entry, ms') <- pop ms
      case entry of
        -- RETFUN
        (Just (ApplyToArgs addrs)) ->
          let (env', args') = extendEnv env addrs
           in return $ tick $ setCode ms' (Eval (App (Ref ref) args') env')
        -- RETFUN
        _ -> return $ tick $ setCode ms (Eval (App (Ref ref) mempty) env)
    -- RET
    StgNat n -> return $ tick $ setCode ms (ReturnLit n)

-- | Step repeatedly until the terminated flag is set
run :: MachineState -> IO MachineState
run = iterateUntilM machineTerminated step
