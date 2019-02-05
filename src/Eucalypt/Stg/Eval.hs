{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
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
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Word
import Eucalypt.Stg.Address (allocate, peek, poke)
import Eucalypt.Stg.CallStack
import Eucalypt.Stg.Error
import Eucalypt.Stg.GlobalInfo (gref)
import Eucalypt.Stg.Intrinsics
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Syn
import Prelude hiding (log)

pattern Empty :: Seq.Seq a
pattern Empty <- (Seq.viewl -> Seq.EmptyL)


-- | Allocate a PAP to record args provided so far and the function
-- info already discovered
allocPartial :: ValVec -> MachineState -> LambdaForm -> ValVec -> IO Address
allocPartial le ms lf xs = allocate pap
  where
    pap =
      PartialApplication
        { papCode = lf
        , papEnv = le
        , papArgs = xs
        , papArity = fromIntegral a
        , papCallStack = machineCallStack ms
        , papMeta = MetadataPassThrough
        }
    a = fromIntegral (lamBound lf) - envSize xs

-- | Push an ApplyToArgs continuation on the stack
pushApplyToArgs :: MonadThrow m => MachineState -> ValVec -> m MachineState
pushApplyToArgs ms xs = return $ push ms (ApplyToArgs xs)

-- | Push an update continuation
pushUpdate ::
     MonadThrow m
  => MachineState
  -> Address -- ^ address to update
  -> HeapObjectMetadata -- ^ metadata override or pass through
  -> m MachineState
pushUpdate ms a md = return $ push ms (Update a md)

-- | Branch expressions expect to find their args as the top entries
-- in the environment (and right now the compiler needs to work out
-- where...) and if they expect a metadata argument the arity will be
-- one more than the arity of the constructor.
--
-- e.g. if deconstructing cons, a branch which specifies arity 2 gets
-- head and tail, a branch which specifies arity 3 gets head, tail and
-- metadata.
selectBranch :: BranchTable -> Tag -> Maybe (Word64, StgSyn)
selectBranch (BranchTable bs _) t = Map.lookup t bs

-- | Halt the machine
terminate :: MachineState -> MachineState
terminate ms@MachineState{} = ms {machineTerminated = True}

-- | Call a lambda form by putting args in environment and rewiring
-- refs then evaluating.
call :: MonadThrow m => ValVec -> MachineState -> LambdaForm -> ValVec -> m Code
call env _ms code addrs = return (Eval (lamBody code) (env <> addrs))

resolveAndCall ::
     MonadThrow m
  => MachineState
  -> ValVec
  -> ValVec
  -> LambdaForm
  -> SynVec
  -> m Code
resolveAndCall ms resolveEnv callEnv lf xs =
  let args = values (resolveEnv, ms) (nativeToValue <$> xs)
   in call callEnv ms lf args

-- | Apply closure when we have the exact number of arguments
applyExact ::
     MonadThrow m
  => MachineState
  -> ValVec
  -> ValVec
  -> LambdaForm
  -> SynVec
  -> m MachineState
applyExact ms env le lf xs = do
  code <- resolveAndCall ms env le lf xs
  return $ ms {machineCode = code} //? "EXACT"



-- | Apply closure when we have too many arguments
applyOver ::
     MonadThrow m
  => MachineState
  -> Ref
  -> Word64
  -> CallStack
  -> ValVec
  -> ValVec
  -> LambdaForm
  -> SynVec
  -> m MachineState
applyOver ms ref _arity _cs env _le LambdaForm {lamBody = (App (Con _) _)} xs =
  if refCount xs == 1
    then return $ ms {machineCode = code} //? "CALLBLOCK"
    else throwIn ms AppliedDataStructureToMoreThanOneArgument
  where
    code = Eval (App (Ref $ gref "MERGE") (xs <> refs [ref])) env
applyOver ms _ref arity cs env le lf xs = do
  let (enough, over) =
        splitVecAt arity $ values (env, ms) (nativeToValue <$> xs)
  ms' <- pushApplyToArgs ms over
  code <- call le ms' lf enough
  return $ ms' {machineCode = code, machineCallStack = cs} //? "CALLK"



-- | Partially apply closure when we have too few arguments
applyUnder ::
     (MonadIO m, MonadThrow m)
  => MachineState
  -> Address
  -> ValVec
  -> ValVec
  -> LambdaForm
  -> SynVec
  -> m MachineState
applyUnder ms addr _env _le _lf xs
  | refCount xs == 0 = return $ ms {machineCode = ReturnFun addr} //? "PAP2-0"
applyUnder ms _addr env le lf xs = do
  addr <-
    liftIO $ allocPartial le ms lf $ values (env, ms) (nativeToValue <$> xs)
  return $ ms {machineCode = ReturnFun addr} //? "PAP2"



-- | Apply partial application when we have the exact number of arguments
applyPartialExact ::
     MonadThrow m
  => MachineState
  -> CallStack
  -> ValVec
  -> ValVec
  -> ValVec
  -> LambdaForm
  -> SynVec
  -> m MachineState
applyPartialExact ms cs env args le lf xs = do
  let remainder = values (env, ms) (nativeToValue <$> xs)
  code <- call le ms lf (args <> remainder)
  return $
    ms
      { machineCode = code
      , machineCallStack = cs
      } //? "PCALL EXACT"



-- | Apply partial application when we have too many arguments
applyPartialOver ::
     MonadThrow m
  => MachineState
  -> Word64
  -> CallStack
  -> ValVec
  -> ValVec
  -> ValVec
  -> LambdaForm
  -> SynVec
  -> m MachineState
applyPartialOver ms arity cs env args le lf xs = do
  let (enough, over) =
        splitVecAt arity $ values (env, ms) (nativeToValue <$> xs)
  ms' <- pushApplyToArgs ms over
  code <- call le ms' lf (args <> enough)
  return $ ms' {machineCode = code, machineCallStack = cs} //? "PCALLK"



-- | Partially apply closure when we have too few arguments
applyPartialUnder ::
     (MonadIO m, MonadThrow m)
  => MachineState
  -> Address
  -> ValVec
  -> ValVec
  -> ValVec
  -> LambdaForm
  -> SynVec
  -> m MachineState
applyPartialUnder ms addr _env _args _le _lf rs
  | refCount rs == 0 =
    return $ ms {machineCode = ReturnFun addr} //? "PCALL PAP2-0"
applyPartialUnder ms _addr env args le lf xs = do
  let fresh = values (env, ms) (nativeToValue <$> xs)
  addr <- liftIO $ allocPartial le ms lf (args <> fresh)
  return $ ms {machineCode = ReturnFun addr} //? "PCALL PAP2"



-- | Main machine step function
step :: (MonadIO m, MonadThrow m) => MachineState -> m MachineState


step ms@MachineState {machineTerminated = True} =
  prepareStep "TERM" ms >> throwIn ms SteppingTerminated



-- | APPLY
step ms0@MachineState {machineCode = (Eval (App f xs) env)} = {-# SCC "EvalApp" #-} do
  ms <- prepareStep "EVAL APP" ms0
  let len = refCount xs
  case f of
    Ref r -> do
      addr <- resolveHeapObject env ms r
      obj <- liftIO $ peek addr
      case obj of
        Closure lf@LambdaForm {lamBound = ar} le cs _meta ->
          case compare (fromIntegral len) ar of
            EQ -> applyExact ms env le lf xs
            GT -> applyOver ms r ar cs env le lf xs
            LT -> applyUnder ms addr env le lf xs
        PartialApplication lf le args ar cs _meta ->
          case compare (fromIntegral len) ar of
            EQ -> applyPartialExact ms cs env args le lf xs
            GT -> applyPartialOver ms ar cs env args le lf xs
            LT -> applyPartialUnder ms addr env args le lf xs
        BlackHole -> throwIn ms EnteredBlackHole
    -- must be saturated
    Con t -> do
      let env' = values (env, ms) (nativeToValue <$> xs)
      return $ setCode ms (ReturnCon t env' Nothing)
    -- must be saturated
    Intrinsic i ->
      let mf = intrinsicFunction i
       in liftIO $ mf ms $ values (env, ms) (nativeToValue <$> xs)



-- | LET
step ms0@MachineState {machineCode = (Eval (Let pcs body) env)} = {-# SCC "EvalLet" #-} do
  ms <- prepareStep "EVAL LET" ms0
  addrs <- liftIO $ traverse (allocClosure env ms) pcs
  let addrVec = StgAddr <$> fromSeq addrs
  let env' = env <> addrVec
  return $ setCode ms (Eval body env')



-- | LET (recursive)
step ms0@MachineState {machineCode = (Eval (LetRec pcs body) env)} = {-# SCC "EvalLetRec" #-} do
  ms <- prepareStep "EVAL LETREC" ms0
  addrs <- liftIO $ sequenceA $ replicate (length pcs) (allocate BlackHole)
  let env' = env <> (toVec . map StgAddr) addrs
  closures <- traverse (buildClosure env' ms) pcs
  liftIO $ zipWithM_ poke addrs (toList closures)
  return $ setCode ms (Eval body env')



-- | CASE
step ms0@MachineState {machineCode = (Eval (Case syn k) env)} = {-# SCC "Case" #-} do
  ms <- prepareStep "EVAL CASE" ms0
  return $ setCode (push ms (Branch k env)) (Eval syn env)



-- | ReturnCon - returns a data structure into a BranchTable branch
step ms0@MachineState {machineCode = (ReturnCon t xs meta)} = {-# SCC "ReturnCon" #-} do
  ms <- prepareStep "RETURNCON" ms0
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch k le)) ->
      case selectBranch k t of
        (Just (branchArity, expr)) -> do
          env <- argsToEnv ms' le (fromIntegral branchArity) xs
          return $ setCode ms' (Eval expr env)
        Nothing -> do
          addr <- allocateForDefault ms'
          case defaultBranch k of
            (Just expr) ->
              return $ setCode ms' (Eval expr (le <> singleton (StgAddr addr)))
            Nothing -> throwIn ms' NoBranchFound
    (Just (Update a storedMeta)) -> do
      let newMeta = asMeta meta <> storedMeta
      updateAddr ms' a t xs newMeta
      return . setRule "UPDATE" $
        setCode ms' (ReturnCon t xs (fromMeta newMeta))
    (Just (ApplyToArgs addrs)) -> do
      addr <- allocateForDefault ms'
      let (env', args') = extendEnv mempty $ singleton (StgAddr addr) <> addrs
      let (f, xs') = headAndTail args'
      return $ setCode ms' (Eval (App (Ref f) xs') env')
    Nothing -> return $ terminate ms'
  where
    argsToEnv ms le expectedArity args =
      if envSize args < expectedArity
        then return $ le <> args <> toVec [fromMaybe (retrieveGlobal ms "KEMPTYBLOCK") meta]
        else return (le <> args)
    allocateForDefault ms =
      liftIO $
      allocate
        (Closure
           (value_ (App (Con t) (range 0 (envSize xs))))
           xs
           (machineCallStack ms) $
         maybe MetadataPassThrough MetadataValue meta)
    updateAddr ms a tag args md =
      liftIO $
      poke
        a
        (Closure
           (standardConstructor (fromIntegral (envSize args)) tag)
           args
           (machineCallStack ms)
           md)



-- | ReturnLit - returns a native value to a default handler in a
-- branch table (when forcing a value) or allocates to update.
step ms0@MachineState {machineCode = (ReturnLit nat meta)} = {-# SCC "ReturnLit" #-} do
  ms <- prepareStep "RETURNLIT" ms0
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch k le)) ->
      case defaultBranch k of
        (Just expr) ->
          return $
          setCode ms' (Eval expr (le <> singleton (StgNat nat meta)))
        Nothing -> throwIn ms' NoDefaultBranchForNativeReturn
    (Just (Update a storedMeta)) -> do
      let newMeta = asMeta meta <> storedMeta
      liftIO $
        poke a (Closure (value_ (Atom (V nat))) mempty mempty newMeta)
      return . setRule "UPDATELIT" $
        setCode ms' (ReturnLit nat (fromMeta newMeta))
    (Just (ApplyToArgs _)) -> throwIn ms' ArgInsteadOfBranchTable
    Nothing -> return $ terminate ms'



-- | ReturnFun - returns a callable into either a continuation that
-- will apply it or a case expressions that can inspect it (closed?)
step ms0@MachineState {machineCode = (ReturnFun r)} = {-# SCC "ReturnFun" #-} do
  ms <- prepareStep "RETURNFUN" ms0
  (entry, ms') <- pop ms
  case entry of
    (Just (ApplyToArgs addrs)) ->
      let (env', args') = extendEnv mempty $ singleton (StgAddr r) <> addrs
          (f, xs) = headAndTail args'
       in return $ setCode ms' (Eval (App (Ref f) xs) env')
    -- RETFUN into case default... (for forcing lambda-valued exprs)
    (Just (Branch (BranchTable _ (Just expr)) le)) ->
      return $ setCode ms' (Eval expr (le <> singleton (StgAddr r)))
    (Just (Update a storedMeta)) -> do
      liftIO $
        poke
          a
          (Closure
             (value_ (Atom (L 0)))
             (singleton (StgAddr r))
             (machineCallStack ms)
             storedMeta)
      return . setRule "UPDATEFN" $ ms'
    _ ->
      return $
      setCode ms' (Eval (App (Ref $ L 0) mempty) (singleton (StgAddr r)))


-- TODO: Pull CON and THUNK objects out of Closure to clean this up

-- | In most cases, we punt on to Eval App which should cause
-- ReturnCon or ReturnLit when we reach a value
step ms0@MachineState {machineCode = (Eval (Atom ref) env)} = {-# SCC "EvalAtom" #-} do
  ms <- prepareStep "EVAL ATOM" ms0
  let v = value (env, ms) $ nativeToValue <$> ref
  case v of
    StgAddr addr -> do
      obj <- liftIO $ peek addr
      case obj of
        Closure LambdaForm {lamUpdate = True, lamBody = code} le cs meta ->
          setRule "THUNK" <$> pushAndEval ms addr code le cs meta
        Closure LambdaForm {lamBound = 0, lamBody = code} le cs meta ->
          case meta of
            MetadataBlank ->
              setRule "BLANKMETA" <$> pushAndEval ms addr code le cs meta
            MetadataValue _ ->
              setRule "SETMETA" <$> pushAndEval ms addr code le cs meta
            MetadataPassThrough ->
              return $
              setCode ms (Eval (App (Ref $ L 0) mempty) (singleton v))
        PartialApplication LambdaForm {lamBody = code} le _args 0 cs meta ->
          case meta of
            MetadataBlank ->
              setRule "BLANKMETAPAP" <$> pushAndEval ms addr code le cs meta
            MetadataValue _ ->
              setRule "SETMETAPAP" <$> pushAndEval ms addr code le cs meta
            MetadataPassThrough ->
              return $
              setCode ms (Eval (App (Ref $ L 0) mempty) (singleton v))
        Closure {} ->
          (return . setRule "RETURNFUN" . (`setCode` ReturnFun addr)) ms
        PartialApplication {} ->
          (return . setRule "RETURNFUN-PAP" . (`setCode` ReturnFun addr)) ms
        BlackHole -> throwIn ms EnteredBlackHole
    StgNat n meta -> return $ setCode ms (ReturnLit n meta)
  where
    pushAndEval ms addr code le cs meta = do
      ms' <- pushUpdate ms addr meta
      liftIO $ poke addr BlackHole
      (return . setCallStack cs) $ setCode ms' (Eval code le)


-- | Append an annotation to the call stack
step ms0@MachineState {machineCode = (Eval (Ann s smid expr) env)} = {-# SCC "EvalAnn" #-} do
  ms <- prepareStep "ANN" ms0
  return . appendCallStack (s, smid) $ setCode ms (Eval expr env)

-- | Step repeatedly until the terminated flag is set
run :: (MonadIO m, MonadThrow m) => MachineState -> m MachineState
run = iterateUntilM machineTerminated step
