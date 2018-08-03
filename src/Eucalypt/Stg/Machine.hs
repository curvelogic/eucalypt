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
import Data.Foldable (toList)
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
  | NonAddressStackValue
  | NonNativeStackValue
  | LiteralUpdate
  | NativeContinuationForCon
  | ConContinuationForNative
  | NoBranchFound
  | PopEmptyStack
  | EnteredBlackHole
  | ArgInsteadOfContinuation
  | ArgInsteadOfNativeContinuation
  | StackIndexOutOfRange
  | EnvironmentIndexOutOfRange
  | IntrinsicIndexOutOfRange
  | SteppingTerminated
  deriving (Typeable, Show, Eq)

instance Exception StgException

-- | A (possibly updateable) closure using real mutability.
newtype Address =
  Address (IORef HeapObject)
  deriving (Eq)

instance Show Address where
  show _ = "0x?"

allocate :: HeapObject -> IO Address
allocate obj = Address <$> newIORef obj

poke :: Address -> HeapObject -> IO ()
poke (Address r) = writeIORef r

peek :: Address -> IO HeapObject
peek (Address r) = readIORef r

-- | Values on the stack or in environments can be addresses or
-- primitives.
data StackValue
  = StackAddr Address
  | StackNat Native
  deriving (Eq, Show)

instance StgPretty StackValue where
  prettify (StackAddr _) = P.text "<addr>"
  prettify (StackNat n) = prettify n

-- | Anything storable in an 'Address'.
data HeapObject
  = Closure { closureCode :: !LambdaForm
            , closureEnv :: !Env }
  | PartialApplication { papCode :: !LambdaForm
                       , pap :: !Env
                       , papArity :: !Int -- remaining args
                        }
  | BlackHole
  deriving (Eq, Show)

instance StgPretty HeapObject where
  prettify (Closure lf env) = prettify env <> P.space <> prettify lf
  prettify (PartialApplication lf env arity) =
    prettify env <> P.space <> P.parens (P.int arity) <> prettify lf
  prettify BlackHole = P.text "•"

-- | Locals
--
-- Itself is not mutable but it is a vector of Addresses with
-- interior mutability
newtype Env =
  Env (Vector StackValue)
  deriving (Eq, Show)

toEnv :: [StackValue] -> Env
toEnv = Env . Vector.fromList

envSize :: Env -> Word64
envSize (Env v) = fromIntegral $ Vector.length v

instance Semigroup Env where
  (<>) (Env l) (Env r) = Env $ l Vector.++ r

instance Monoid Env where
  mempty = Env mempty
  mappend = (<>)

instance StgPretty Env where
  prettify (Env vs) =
    P.braces $ P.hcat $ P.punctuate P.comma (map prettify (toList vs))

-- | Entry on the frame stack, encoding processing to be done later
-- when a value is available
data StackEntry
  = Branch !Continuation
           !Env
  | NativeBranch !NativeContinuation
  | Update !Address
  | Arg !StackValue
  deriving (Eq, Show)

-- | Currently executing code
data Code
  = Eval !StgSyn
         !Env
  | Enter !Address
  | ReturnCon !Tag
              !Env
  | ReturnLit !Native
  | Terminate
  deriving (Eq, Show)

instance StgPretty Code where
  prettify (Eval e le) =
    P.text "EVAL" <> P.space <> prettify le <> P.space <> prettify e
  prettify (Enter _) = P.text "ENTER" <> P.space <> P.text "<addr>"
  prettify (ReturnCon t le) =
    P.text "RETURNCON" <> P.space <> prettify le <> P.space <>
    P.int (fromIntegral t)
  prettify (ReturnLit n) = P.text "RETURNLIT" <> P.space <> prettify n
  prettify Terminate = P.text "TERMINATE"

-- | Machine state.
--
data MachineState = MachineState
  { machineCode :: Code
  , machineGlobals :: HashMap String StackValue
  , machineStack :: Vector StackEntry
  , machineCounter :: Int
  }

-- | Initialise machine state.
initMachineState :: StgSyn -> HashMap String StackValue -> MachineState
initMachineState stg ge = MachineState (Eval stg (Env mempty)) ge mempty 0

-- | Dump machine state for debugging.
instance Show MachineState where
  show MachineState { machineCode = code
                    , machineGlobals = globals
                    , machineStack = stack
                    } =
    "Code:\n\n" ++
    show code ++
    "\n\nGlobals:\n\n" ++ show globals ++ "\n\nStack:\n\n" ++ show stack

-- | Build a closure from a STG PreClosure
buildClosure ::
     MonadThrow m => Env -> MachineState -> PreClosure -> m HeapObject
buildClosure le ms (PreClosure captures code) =
  Closure code <$> vals le ms captures

-- -- | Allocate new closure.
allocClosure :: Env -> MachineState -> PreClosure -> IO Address
allocClosure le ms cc = buildClosure le ms cc >>= allocate

-- | Resolve a ref against env and machine to get address of
-- HeapObject
resolveHeapObject :: MonadThrow m => Env -> MachineState -> Ref -> m Address
resolveHeapObject env st ref =
  val env st ref >>= \case
    StackAddr r -> return r
    StackNat _ -> throwM NonAddressStackValue

resolveNative :: MonadThrow m => Env -> MachineState -> Ref -> m Native
resolveNative env st ref =
  val env st ref >>= \case
    StackAddr _ -> throwM NonNativeStackValue
    StackNat n -> return n

val :: MonadThrow m => Env -> MachineState -> Ref -> m StackValue
val (Env le) _ (LocalEnv l) =
  case le !? fromIntegral l of
    Just v -> return v
    _ -> throwM EnvironmentIndexOutOfRange
val _ (MachineState _c _g st _) (StackArg l) =
  case st !? (Vector.length st - fromIntegral l - 1) of
    Just (Arg r) -> return r
    Just _ -> throwM NonArgStackEntry
    Nothing -> throwM StackIndexOutOfRange
val _ (MachineState _c g _st _) (Global nm) = return $ g HM.! nm
val _ _ (Literal n) = return $ StackNat n

-- | Resolve a vector of refs against an environment to create
-- environment
vals :: MonadThrow m => Env -> MachineState -> Vector Ref -> m Env
vals le ms refs = Env <$> traverse (val le ms) refs

-- | Push an entry onto the stack
push :: MachineState -> StackEntry -> MachineState
push ms@MachineState {machineStack = st} v =
  ms {machineStack = Vector.snoc st v}

pop :: MonadThrow m => MachineState -> m (Maybe StackEntry, MachineState)
pop ms@MachineState {machineStack = st} =
  if Vector.null st
    then return (Nothing, ms)
    else return (Just $ Vector.head st, ms {machineStack = Vector.tail st})

-- | Set the next instruction
setCode :: MachineState -> Code -> MachineState
setCode ms@MachineState {} c = ms {machineCode = c}

-- | Args are reversed on the stack
pushArgs :: MonadThrow m => Env -> Vector Ref -> MachineState -> m MachineState
pushArgs le xs ms@MachineState {machineStack = stack} = do
  args <- traverse (val le ms) xs
  return $ ms {machineStack = stack Vector.++ Vector.map Arg args}

selectBranch :: MonadThrow m => Continuation -> Tag -> m StgSyn
selectBranch (Continuation bs df) t =
  let syn = (snd <$> Map.lookup t bs) <|> df
   in case syn of
        Just e -> return e
        Nothing -> throwM NoBranchFound

selectNativeBranch :: MonadThrow m => NativeContinuation -> Native -> m StgSyn
selectNativeBranch (NativeContinuation bs df) t =
  let syn = HM.lookup t bs <|> df
   in case syn of
        Just e -> return e
        Nothing -> throwM NoBranchFound

dump :: MachineState -> IO ()
dump MachineState {machineCode = code, machineCounter = i} =
  putStrLn $ P.render $ P.int i <> P.colon <> P.space <> prettify code

tick :: MachineState -> MachineState
tick ms@MachineState {machineCounter = n} = ms {machineCounter = n + 1}

step :: MachineState -> IO MachineState
step ms@MachineState {machineCode = (Eval (App f xs) env)} = do
  dump ms
  case f of
    Ref r -> do
      addr <- resolveHeapObject env ms r
      ms' <- pushArgs env xs ms
      return $ tick $ setCode ms' (Enter addr)
    Con t -> do
      env' <- vals env ms xs
      return $ tick $ setCode ms (ReturnCon t env')
    Intrinsic i ->
      case intrinsics !? i of
        Just mf -> do
          ms' <- pushArgs env xs ms
          tick <$> mf ms'
        Nothing -> throwM IntrinsicIndexOutOfRange
step ms@MachineState {machineCode = (Eval (Let pcs body) env)} = do
  dump ms
  addrs <- traverse (allocClosure env ms) pcs
  let env' = env <> (Env . Vector.map StackAddr) addrs
  return $ tick $ setCode ms (Eval body env')
step ms@MachineState {machineCode = (Eval (LetRec pcs body) env)} = do
  dump ms
  addrs <- sequenceA $ replicate (length pcs) (allocate BlackHole)
  let env' = env <> (toEnv . map StackAddr) addrs
  closures <- traverse (buildClosure env' ms) pcs
  zipWithM_ poke addrs (toList closures)
  return $ tick $ setCode ms (Eval body env')
step ms@MachineState {machineCode = (Eval (Case syn k) env)} = do
  dump ms
  return $ tick $ setCode (push ms (Branch k env)) (Eval syn env)
step ms@MachineState {machineCode = (Eval (CaseLit syn k) env)} = do
  dump ms
  return $ tick $ setCode (push ms (NativeBranch k)) (Eval syn env)
step ms@MachineState {machineCode = (Eval (Lit n) _)} = do
  dump ms
  return $ tick $ setCode ms (ReturnLit n)
step ms@MachineState {machineCode = (Enter a)} = do
  dump ms
  obj <- peek a
  case obj of
    (Closure (LambdaForm _f _b False body) env) ->
      return $ tick $ setCode ms (Eval body env) -- args left on stack where
      -- code expects them
    (Closure (LambdaForm _f _b True body) env) ->
      return $ tick $ setCode (push ms (Update a)) (Eval body env)
    PartialApplication {} -> undefined
    BlackHole -> throwM EnteredBlackHole
step ms@MachineState {machineCode = (ReturnCon t xs)} = do
  dump ms
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch k le)) ->
      selectBranch k t >>= \c -> return $ tick $ setCode ms' (Eval c (le <> xs))
    (Just (NativeBranch _)) -> throwM NativeContinuationForCon
    (Just (Update a)) -> do
      poke a (Closure (standardConstructor (envSize xs) t) xs)
      return $ tick ms'
    (Just (Arg _)) -> throwM ArgInsteadOfContinuation
    Nothing -> return $ tick $ setCode ms' Terminate
step ms@MachineState {machineCode = (ReturnLit nat)} = do
  dump ms
  (entry, ms') <- pop ms
  case entry of
    (Just (Branch _ _)) -> throwM ConContinuationForNative
    (Just (NativeBranch k)) ->
      selectNativeBranch k nat >>= \c ->
        return $ tick $ setCode ms' (Eval c mempty)
    (Just (Update _)) -> throwM LiteralUpdate
    (Just (Arg _)) -> throwM ArgInsteadOfNativeContinuation
    Nothing -> return $ tick $ setCode ms' Terminate
step ms@MachineState {machineCode = Terminate} = do
  dump ms
  throwM SteppingTerminated

-- | Step repeatedly
run :: MachineState -> IO MachineState
run = iterateUntilM terminated step
  where
    terminated MachineState {machineCode = Terminate} = True
    terminated _ = False

-- ======================================================================
-- Intrinsics
-- ======================================================================
-- a builtin to yield yaml events
--
-- arity 1
yield :: MachineState -> IO MachineState
yield ms = do
  (arg, ms') <- pop ms
  case arg of
    Just (Arg (StackAddr a)) -> peek a >>= putStrLn . P.render . prettify
    Just (Arg (StackNat n)) -> print n
    _ -> throwM StackIndexOutOfRange
  return $ setCode ms' Terminate

intrinsics :: Vector (MachineState -> IO MachineState)
intrinsics = Vector.fromList [yield]
