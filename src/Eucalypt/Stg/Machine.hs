{-# LANGUAGE FlexibleContexts, LambdaCase #-}

{-|
Module      : Eucalypt.Stg.Machine
Description : Spineless tagless G-machine
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}
module Eucalypt.Stg.Machine where

import Control.Applicative
import Control.Exception.Safe
import Control.Monad (zipWithM_)
import Control.Monad.Loops (iterateUntilM)
import Control.Monad.State
import Data.Foldable (toList, traverse_)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.IORef
import qualified Data.Map as Map
import Data.Vector (Vector, (!?))
import qualified Data.Vector as Vector
import Data.Word
import Eucalypt.Stg.Syn
import Prelude hiding (log)
import qualified Text.PrettyPrint as P

data StgException
  = NonArgStackEntry
  | NonAddressStgValue
  | NonNativeStgValue
  | LiteralUpdate
  | NativeBranchTableForCon
  | ConBranchTableForNative
  | NoBranchFound
  | PopEmptyStack
  | EnteredBlackHole
  | ArgInsteadOfBranchTable
  | ArgInsteadOfNativeBranchTable
  | StackIndexOutOfRange
  | EnvironmentIndexOutOfRange
  | IntrinsicIndexOutOfRange
  | SteppingTerminated
  | AttemptToResolveBoundArg
  deriving (Typeable, Show, Eq)

instance Exception StgException

-- | A mutable refence to a heap object
newtype Address =
  Address (IORef HeapObject)
  deriving (Eq)

instance Show Address where
  show _ = "0x?"

-- | Allocate a new heap object, return address
allocate :: HeapObject -> IO Address
allocate obj = Address <$> newIORef obj

-- | Replace the heap object at this address
poke :: Address -> HeapObject -> IO ()
poke (Address r) = writeIORef r

-- | Retrieve the heap object at this address
peek :: Address -> IO HeapObject
peek (Address r) = readIORef r

-- | Values on the stack or in environments can be addresses or
-- primitives. All 'Ref's are resolved to 'StgValues' within the
-- machine.
data StgValue
  = StgAddr Address
  | StgNat Native
  deriving (Eq, Show)

instance StgPretty StgValue where
  prettify (StgAddr _) = P.text "<addr>"
  prettify (StgNat n) = prettify n

-- | Anything storable in an 'Address'.
data HeapObject
  = Closure { closureCode :: !LambdaForm
            , closureEnv :: !ValVec }
  | PartialApplication { papCode :: !LambdaForm
                       , papEnv :: !ValVec
                       , papArgs :: !ValVec
                       , papArity :: !Word64 }
  | BlackHole
  deriving (Eq, Show)

instance StgPretty HeapObject where
  prettify (Closure lf env) = prettify env <> P.space <> prettify lf
  prettify (PartialApplication lf env args arity) =
    prettify env <> P.space <>
    P.parens (prettify args <> P.text "..." <> P.int (fromIntegral arity)) <>
    prettify lf
  prettify BlackHole = P.text "•"

-- | Vector of values, used for both local environment and arrays of
-- resolved arguments.
newtype ValVec =
  ValVec (Vector StgValue)
  deriving (Eq, Show)

toValVec :: [StgValue] -> ValVec
toValVec = ValVec . Vector.fromList

envSize :: ValVec -> Word64
envSize (ValVec v) = fromIntegral $ Vector.length v

singleton :: StgValue -> ValVec
singleton = ValVec . Vector.singleton

extendEnv :: ValVec -> ValVec -> (ValVec, RefVec)
extendEnv env args = (env', refs)
  where
    envlen = envSize env
    arglen = envSize args
    env' = env <> args
    refs = locals envlen (arglen + envlen)

instance Semigroup ValVec where
  (<>) (ValVec l) (ValVec r) = ValVec $ l Vector.++ r

instance Monoid ValVec where
  mempty = ValVec mempty
  mappend = (<>)

instance StgPretty ValVec where
  prettify (ValVec vs) =
    P.braces $ P.hcat $ P.punctuate P.comma (map prettify (toList vs))

-- | Entry on the frame stack, encoding processing to be done later
-- when a value is available
data Continuation
  = Branch !BranchTable
           !ValVec
  | NativeBranch !NativeBranchTable
  | Update !Address
  | ApplyToArgs !ValVec
  deriving (Eq, Show)

instance StgPretty Continuation where
  prettify (Branch _ _) = P.text "Br"
  prettify (NativeBranch _) = P.text "NBr"
  prettify (Update _) = P.text "Up"
  prettify (ApplyToArgs _) = P.text "Ap"

-- | Currently executing code
data Code
  = Eval !StgSyn
         !ValVec
  | ReturnCon !Tag
              !ValVec
  | ReturnLit !Native
  deriving (Eq, Show)

instance StgPretty Code where
  prettify (Eval e le) =
    P.text "EVAL" <> P.space <> prettify le <> P.space <> prettify e
  prettify (ReturnCon t binds) =
    P.text "RETURNCON" <> P.space <> P.int (fromIntegral t) <> P.space <>
    prettify binds
  prettify (ReturnLit n) = P.text "RETURNLIT" <> P.space <> prettify n

-- | Machine state.
--
data MachineState = MachineState
  { machineCode :: Code
  , machineGlobals :: HashMap String StgValue
  , machineStack :: Vector Continuation
  , machineCounter :: Int
  , machineTerminated :: Bool
  , machineTrace :: MachineState -> IO ()
  }

-- | Initialise machine state.
initMachineState :: StgSyn -> HashMap String StgValue -> MachineState
initMachineState stg ge =
  MachineState
    { machineCode = Eval stg mempty
    , machineGlobals = ge
    , machineStack = mempty
    , machineCounter = 0
    , machineTerminated = False
    , machineTrace = \_ -> return ()
    }

-- | Initialise machine state with a trace function that dumps state
-- every step
initDebugMachineState :: StgSyn -> HashMap String StgValue -> MachineState
initDebugMachineState stg ge = ms { machineTrace = dump }
  where ms = initMachineState stg ge

-- | Dump machine state for debugging.
instance StgPretty MachineState where
  prettify MachineState { machineCode = code
                        , machineGlobals = _globals
                        , machineStack = stack
                        , machineCounter = counter
                        } =
    P.int counter <> P.colon <> P.space <>
    P.parens (P.hcat (P.punctuate P.colon (map prettify (toList stack)))) <>
    P.space <>
    prettify code

-- | Build a closure from a STG PreClosure
buildClosure ::
     MonadThrow m => ValVec -> MachineState -> PreClosure -> m HeapObject
buildClosure le ms (PreClosure captures code) =
  Closure code <$> vals le ms captures

-- | Allocate new closure.
allocClosure :: ValVec -> MachineState -> PreClosure -> IO Address
allocClosure le ms cc = buildClosure le ms cc >>= allocate

-- | Allocate a PAP to record args provided so far and the function
-- info already discovered
allocPartial :: ValVec -> MachineState -> LambdaForm -> ValVec -> IO Address
allocPartial le _ms lf xs = allocate pap
  where
    pap =
      PartialApplication
        {papCode = lf, papEnv = le, papArgs = xs, papArity = arity}
    arity = fromIntegral (_free lf) - envSize xs

-- | Resolve a ref against env and machine to get address of
-- HeapObject
resolveHeapObject :: MonadThrow m => ValVec -> MachineState -> Ref -> m Address
resolveHeapObject env st ref =
  val env st ref >>= \case
    StgAddr r -> return r
    StgNat _ -> throwM NonAddressStgValue

resolveNative :: MonadThrow m => ValVec -> MachineState -> Ref -> m Native
resolveNative env st ref =
  val env st ref >>= \case
    StgAddr _ -> throwM NonNativeStgValue
    StgNat n -> return n

-- | Resolve environment references against local and global
-- environments. If a ref is still a BoundArg at the point it is
-- resolved, AttemptToResolveBoundArg will be thrown. Args are
-- resolved and recorded in environment for use.
val :: MonadThrow m => ValVec -> MachineState -> Ref -> m StgValue
val (ValVec le) _ (Local l) =
  case le !? fromIntegral l of
    Just v -> return v
    _ -> throwM EnvironmentIndexOutOfRange
val _ _ (BoundArg _) = throwM AttemptToResolveBoundArg
val _ MachineState {machineGlobals = g} (Global nm) = return $ g HM.! nm
val _ _ (Literal n) = return $ StgNat n

-- | Resolve a vector of refs against an environment to create
-- environment
vals :: MonadThrow m => ValVec -> MachineState -> Vector Ref -> m ValVec
vals le ms refs = ValVec <$> traverse (val le ms) refs

-- | Push a continuation onto the stack
push :: MachineState -> Continuation -> MachineState
push ms@MachineState {machineStack = st} v =
  ms {machineStack = Vector.snoc st v}

-- | Pop a continuation off the stack
pop :: MonadThrow m => MachineState -> m (Maybe Continuation, MachineState)
pop ms@MachineState {machineStack = st} =
  if Vector.null st
    then return (Nothing, ms)
    else return (Just $ Vector.head st, ms {machineStack = Vector.tail st})

-- | Set the next instruction
setCode :: MachineState -> Code -> MachineState
setCode ms@MachineState {} c = ms {machineCode = c}

-- | Push an ApplyToArgs continuation on the stack
pushApplyToArgs :: MonadThrow m => MachineState -> ValVec -> m MachineState
pushApplyToArgs ms@MachineState {machineStack = stack} xs =
  return $ ms {machineStack = stack `Vector.snoc` ApplyToArgs xs}

selectBranch :: MonadThrow m => BranchTable -> Tag -> m StgSyn
selectBranch (BranchTable bs df) t =
  let syn = (snd <$> Map.lookup t bs) <|> df
   in case syn of
        Just e -> return e
        Nothing -> throwM NoBranchFound

selectNativeBranch :: MonadThrow m => NativeBranchTable -> Native -> m StgSyn
selectNativeBranch (NativeBranchTable bs df) t =
  let syn = HM.lookup t bs <|> df
   in case syn of
        Just e -> return e
        Nothing -> throwM NoBranchFound

-- | Call the machine's trace function
traceOut :: MachineState -> IO ()
traceOut ms@MachineState {machineTrace = tr} = tr ms

-- | A debug dump to use as machine's trace function
dump :: MachineState -> IO ()
dump ms = putStrLn $ P.render $ prettify ms

-- | Increase counters
tick :: MachineState -> MachineState
tick ms@MachineState {machineCounter = n} = ms {machineCounter = n + 1}

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
        Closure lf@LambdaForm {_bound = arity} le ->
          case compare (fromIntegral len) arity
            -- EXACT
                of
            EQ -> tick . setCode ms <$> (vals env ms xs >>= call le ms lf)
            -- CALLK
            GT ->
              let (enough, over) = Vector.splitAt (fromIntegral arity) xs
               in vals env ms over >>= pushApplyToArgs ms >>= \s ->
                    tick . setCode s <$> (vals env ms enough >>= call le ms lf)
            -- PAP2
            LT ->
              vals env ms xs >>= allocPartial le ms lf >>= \a ->
                return $
                tick $
                setCode ms (Eval (Atom (Local 0)) (singleton (StgAddr a)))
        -- PCALL
        PartialApplication code le args arity ->
          case compare (fromIntegral len) arity of
            EQ ->
              vals env ms xs >>= \as ->
                tick . setCode ms <$> call le ms code (args <> as)
            GT ->
              let (enough, over) = Vector.splitAt (fromIntegral arity) xs
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
      case intrinsics !? i of
        Just mf -> do
          args <- vals env ms xs
          tick <$> mf ms args
        Nothing -> throwM IntrinsicIndexOutOfRange
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
  return $ tick $ setCode (push ms (NativeBranch k)) (Eval syn env)
step ms@MachineState {machineCode = (ReturnCon t xs)} = do
  traceOut ms
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch k le)) ->
      selectBranch k t >>= \c -> return $ tick $ setCode ms' (Eval c (le <> xs))
    (Just (NativeBranch _)) -> throwM NativeBranchTableForCon
    (Just (Update a)) -> do
      poke a (Closure (standardConstructor (envSize xs) t) xs)
      return $ tick ms'
    (Just (ApplyToArgs _)) -> throwM ArgInsteadOfBranchTable
    Nothing -> return $ tick $ terminate ms'
step ms@MachineState {machineCode = (ReturnLit nat)} = do
  traceOut ms
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch _ _)) -> throwM ConBranchTableForNative
    (Just (NativeBranch k)) ->
      selectNativeBranch k nat >>= \c ->
        return $ tick $ setCode ms' (Eval c mempty)
    (Just (Update _)) -> throwM LiteralUpdate
    (Just (ApplyToArgs _)) -> throwM ArgInsteadOfNativeBranchTable
    Nothing -> return $ tick $ terminate ms'
step ms@MachineState {machineCode = (Eval (Atom ref) env)} = do
  traceOut ms
  v <- val env ms ref
  case v of
    StgAddr _ -> do
      (entry, ms') <- pop ms
      case entry of
        (Just (ApplyToArgs addrs)) ->
          let (env', args') = extendEnv env addrs
           in return $ tick $ setCode ms' (Eval (App (Ref ref) args') env')
        _ -> return $ tick $ setCode ms (Eval (App (Ref ref) mempty) env)
    StgNat n -> return $ tick $ setCode ms (ReturnLit n)

-- | Step repeatedly
run :: MachineState -> IO MachineState
run = iterateUntilM machineTerminated step


-- ======================================================================
-- Intrinsics
-- ======================================================================
-- a builtin to yield yaml events
--
-- arity 1
yield :: MachineState -> ValVec -> IO MachineState
yield ms (ValVec args) = do
  traverse_ echo args
  return $ terminate ms
  where
    echo arg =
      case arg of
        (StgAddr a) -> peek a >>= putStrLn . P.render . prettify
        (StgNat n) -> print n

intrinsics :: Vector (MachineState -> ValVec -> IO MachineState)
intrinsics = Vector.fromList [yield]
