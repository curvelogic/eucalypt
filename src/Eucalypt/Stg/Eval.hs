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
import Data.Maybe (fromMaybe)
import qualified Data.Vector as Vector
import Data.Word
import Eucalypt.Stg.Error
import Eucalypt.Stg.Intrinsics
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Syn
import Prelude hiding (log)

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
        , papArity = a
        , papCallStack = machineCallStack ms
        , papMeta = MetadataPassThrough
        }
    a = fromIntegral (_bound lf) - envSize xs

-- | Push a continuation onto the stack
push :: MachineState -> Continuation -> MachineState
push ms@MachineState {machineStack = st} k =
  let stackElement = StackElement k (machineCallStack ms)
   in ms {machineStack = Vector.snoc st stackElement}

-- | Pop a continuation off the stack
pop :: MonadThrow m => MachineState -> m (Maybe Continuation, MachineState)
pop ms@MachineState {machineStack = st} =
  if Vector.null st
    then return (Nothing, ms)
    else let StackElement k cs = Vector.last st
          in return
               ( Just k
               , ms {machineStack = Vector.init st, machineCallStack = cs})

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
selectBranch (BranchTable bs _ _) t = Map.lookup t bs

-- | Match a native branch table alternative, return the next
-- expression to eval
selectNativeBranch :: BranchTable -> Native -> Maybe StgSyn
selectNativeBranch (BranchTable _ bs _) n = HM.lookup n bs

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
step :: (MonadIO m, MonadThrow m) => MachineState -> m MachineState
step ms@MachineState {machineTerminated = True} =
  prepareStep "TERM" ms >> throwIn ms SteppingTerminated
step ms0@MachineState {machineCode = (Eval (App f xs) env)} = do
  ms <- prepareStep "EVAL APP" ms0
  let len = length xs
  case f of
    Ref r -> do
      addr <- resolveHeapObject env ms r
      obj <- liftIO $ peek addr
      case obj of
        Closure lf@LambdaForm {_bound = ar} le cs _meta ->
          case compare (fromIntegral len) ar
            -- EXACT
                of
            EQ ->
              setCallStack cs . setRule "EXACT" . setCode ms <$>
              (vals env ms xs >>= call le ms lf)
            -- CALLK
            GT ->
              let (enough, over) = Vector.splitAt (fromIntegral ar) xs
               in vals env ms over >>= pushApplyToArgs ms >>= \s ->
                    setCallStack cs . setRule "CALLK" . setCode s <$>
                    (vals env ms enough >>= call le ms lf)
            -- PAP2
            LT ->
              if len == 0
                then return $ (setRule "PAP2-0" . setCode ms) (ReturnFun addr)
                else liftIO $
                     vals env ms xs >>= allocPartial le ms lf >>= \a ->
                       return $ (setRule "PAP2" . setCode ms) (ReturnFun a)
        -- PCALL
        PartialApplication code le args ar cs _meta ->
          case compare (fromIntegral len) ar of
            EQ ->
              vals env ms xs >>= \as ->
                setCallStack cs . setRule "PCALL EXACT" . setCode ms <$>
                call le ms code (args <> as)
            GT ->
              let (enough, over) = Vector.splitAt (fromIntegral ar) xs
               in vals env ms over >>= pushApplyToArgs ms >>= \s ->
                    vals env ms enough >>= \as ->
                      setCallStack cs . setRule "PCALLK" . setCode s <$>
                      call le ms code (args <> as)
            LT ->
              if len == 0
                then return $ -- return fun?
                     (setRule "PCALL PAP2-0" . setCode ms) (ReturnFun addr)
                else liftIO $
                     vals env ms xs >>= \fresh ->
                       allocPartial le ms code (args <> fresh) >>= \a ->
                         return $
                         (setRule "PCALL PAP2" . setCode ms) (ReturnFun a)
        BlackHole -> throwIn ms EnteredBlackHole
    -- must be saturated
    Con t -> do
      env' <- vals env ms xs
      return $ setCode ms (ReturnCon t env' Nothing)
    -- must be saturated
    Intrinsic i ->
      let mf = intrinsicFunction i
       in liftIO $ vals env ms xs >>= mf ms



-- | LET
step ms0@MachineState {machineCode = (Eval (Let pcs body) env)} = do
  ms <- prepareStep "EVAL LET" ms0
  addrs <- liftIO $ traverse (allocClosure env ms) pcs
  let env' = env <> (ValVec . Vector.map StgAddr) addrs
  return $ setCode ms (Eval body env')



-- | LET (recursive)
step ms0@MachineState {machineCode = (Eval (LetRec pcs body) env)} = do
  ms <- prepareStep "EVAL LETREC" ms0
  addrs <- liftIO $ sequenceA $ replicate (length pcs) (allocate BlackHole)
  let env' = env <> (toValVec . map StgAddr) addrs
  closures <- traverse (buildClosure env' ms) pcs
  liftIO $ zipWithM_ poke addrs (toList closures)
  return $ setCode ms (Eval body env')



-- | CASE
step ms0@MachineState {machineCode = (Eval (Case syn k) env)} = do
  ms <- prepareStep "EVAL CASE" ms0
  return $ setCode (push ms (Branch k env)) (Eval syn env)



-- | ReturnCon - returns a data structure into a BranchTable branch
step ms0@MachineState {machineCode = (ReturnCon t xs meta)} = do
  ms <- prepareStep "RETURNCON" ms0
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch k le)) ->
      case selectBranch k t of
        (Just (branchArity, expr)) -> do
          env <- argsToEnv ms' le branchArity xs
          return $ setCode ms' (Eval expr env)
        Nothing -> do
          addr <- allocateForDefault ms' xs meta
          case defaultBranch k of
            (Just expr) ->
              return $ setCode ms' (Eval expr (le <> singleton (StgAddr addr)))
            Nothing -> throwIn ms' NoBranchFound
    (Just (Update a storedMeta)) -> do
      let newMeta = asMeta meta `withMeta` storedMeta
      updateAddr ms' a t xs newMeta
      return . setRule "UPDATE" $
        setCode ms' (ReturnCon t xs (fromMeta newMeta))
    (Just (ApplyToArgs _)) -> throwIn ms' ArgInsteadOfBranchTable
    Nothing -> return $ terminate ms'
  where
    argsToEnv ms le expectedArity args =
      if envSize args < expectedArity
        then do
          e <- globalAddress ms "KEMPTYBLOCK"
          return $ le <> args <> toValVec [fromMaybe e meta]
        else return (le <> args)
    allocateForDefault ms args md =
      liftIO $
      allocate
        (Closure
           (LambdaForm 0 0 False (App (Con t) (locals 0 (envSize args))))
           args
           (machineCallStack ms) $
         maybe MetadataPassThrough MetadataValue md)
    updateAddr ms a tag args md =
      liftIO $
      poke
        a
        (Closure
           (standardConstructor (envSize args) tag)
           args
           (machineCallStack ms)
           md)




-- | ReturnLit - returns a native value to a NativeBranchTable or
-- terminates if none.
step ms0@MachineState {machineCode = (ReturnLit nat meta)} = do
  ms <- prepareStep "RETURNLIT" ms0
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch k le)) ->
      case selectNativeBranch k nat of
        (Just expr) -> return $ setCode ms' (Eval expr le)
        Nothing ->
          case defaultBranch k of
            (Just expr) ->
              return $
              setCode ms' (Eval expr (le <> singleton (StgNat nat Nothing)))
            Nothing -> throwIn ms' NoBranchFound
    (Just (Update a storedMeta)) -> do
      let newMeta = asMeta meta `withMeta` storedMeta
      liftIO $
        poke a (Closure (value_ (Atom (Literal nat))) mempty mempty newMeta)
      return . setRule "UPDATELIT" $
        setCode ms' (ReturnLit nat (fromMeta newMeta))
    (Just (ApplyToArgs _)) -> throwIn ms' ArgInsteadOfNativeBranchTable
    Nothing -> return $ terminate ms'



-- | ReturnFun - returns a callable into either a continuation that
-- will apply it or a case expressions that can inspect it (closed?)
step ms0@MachineState {machineCode = (ReturnFun r)} = do
  ms <- prepareStep "RETURNFUN" ms0
  (entry, ms') <- pop ms
  case entry of
    (Just (ApplyToArgs addrs)) ->
      let (env', args') = extendEnv mempty $ singleton (StgAddr r) <> addrs
       in return $
          setCode
            ms'
            (Eval (App (Ref $ Vector.head args') (Vector.tail args')) env')
    -- RETFUN into case default... (for forcing lambda-valued exprs)
    (Just (Branch (BranchTable _ _ (Just expr)) le)) ->
      return $ setCode ms' (Eval expr (le <> singleton (StgAddr r)))
    (Just (Update a storedMeta)) -> do
      liftIO $
        poke
          a
          (Closure
             (value_ (Atom (Local 0)))
             (singleton (StgAddr r))
             (machineCallStack ms)
             storedMeta)
      return . setRule "UPDATEFN" $ ms'
    _ ->
      return $
      setCode ms' (Eval (App (Ref $ Local 0) mempty) (singleton (StgAddr r)))


-- TODO: Pull CON and THUNK objects out of Closure to clean this up

-- | In most cases, we punt on to Eval App which should cause
-- ReturnCon or ReturnLit when we reach a value
step ms0@MachineState {machineCode = (Eval (Atom ref) env)} = do
  ms <- prepareStep "EVAL ATOM" ms0
  v <- val env ms ref
  case v of
    StgAddr addr -> do
      obj <- liftIO $ peek addr
      case obj of
        Closure LambdaForm {_update = True, _body = code} le cs meta ->
          setRule "THUNK" <$> pushAndEval ms addr code le cs meta
        Closure LambdaForm {_bound = 0, _body = code} le cs meta ->
          case meta of
            MetadataBlank ->
              setRule "BLANKMETA" <$> pushAndEval ms addr code le cs meta
            MetadataValue _ ->
              setRule "SETMETA" <$> pushAndEval ms addr code le cs meta
            MetadataPassThrough ->
              return $
              setCode ms (Eval (App (Ref $ Local 0) mempty) (singleton v))
        PartialApplication LambdaForm {_body = code} le _args 0 cs meta ->
          case meta of
            MetadataBlank ->
              setRule "BLANKMETAPAP" <$> pushAndEval ms addr code le cs meta
            MetadataValue _ ->
              setRule "SETMETAPAP" <$> pushAndEval ms addr code le cs meta
            MetadataPassThrough ->
              return $
              setCode ms (Eval (App (Ref $ Local 0) mempty) (singleton v))
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
step ms0@MachineState {machineCode = (Eval (Ann s expr) env)} = do
  ms <- prepareStep "ANN" ms0
  return . appendCallStack s $ setCode ms (Eval expr env)

-- | Step repeatedly until the terminated flag is set
run :: (MonadIO m, MonadThrow m) => MachineState -> m MachineState
run = iterateUntilM machineTerminated step
