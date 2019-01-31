{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.IO.Class
import Data.Foldable (toList)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Semigroup
import qualified Data.Sequence as Seq
import Data.Word
import Eucalypt.Core.SourceMap
import Eucalypt.Stg.Address
import Eucalypt.Stg.CallStack
import Eucalypt.Stg.Error
import Eucalypt.Stg.Event
import Eucalypt.Stg.Syn
import Prelude hiding (log)
import qualified Text.PrettyPrint as P
import Text.PrettyPrint (($+$), (<+>))


-- | Address type where mutable heap objects are allocated
type Address = AbstractAddress HeapObject

-- | Values on the stack or in environments can be addresses or
-- primitives. All 'Ref's are resolved to 'StgValues' within the
-- machine.
data StgValue
  = StgAddr !Address
  | StgNat !Native !(Maybe StgValue)
  deriving (Eq, Show)

instance StgPretty StgValue where
  prettify (StgAddr _) = P.text "<addr>"
  prettify (StgNat n _) = prettify n

-- | Metadata as referenced from a closure
--
-- References to metadata may be attached to heap objects and natives.
-- This data does not form part of the value, is not evaluated except
-- when explicitly forced and is irrelevant for object equality.
--
-- It can be used to control formatting during render, give hints to
-- evaluation and diagnostic functionality etc.
data HeapObjectMetadata
  = MetadataBlank -- ^ blank whatever metadata is attached to the value
  | MetadataPassThrough -- ^ use whatever metadata is attached to the value
  | MetadataValue !StgValue -- ^ override value's metadata
  deriving (Eq, Show)

instance Semigroup HeapObjectMetadata where
  (<>) _ MetadataBlank = MetadataBlank
  (<>) l MetadataPassThrough = l
  (<>) _ r = r

asMeta :: Maybe StgValue -> HeapObjectMetadata
asMeta Nothing = MetadataBlank
asMeta (Just v) = MetadataValue v

fromMeta :: HeapObjectMetadata -> Maybe StgValue
fromMeta (MetadataValue v) = Just v
fromMeta _ = Nothing

-- | Anything storable in an 'Address'.
data HeapObject
  = Closure { closureCode :: !LambdaForm
            , closureEnv :: !ValVec
            , closureCallStack :: !CallStack
            , closureMeta :: !HeapObjectMetadata }
  | PartialApplication { papCode :: !LambdaForm
                       , papEnv :: !ValVec
                       , papArgs :: !ValVec
                       , papArity :: !Word64
                       , papCallStack :: !CallStack
                       , papMeta :: !HeapObjectMetadata }
  | BlackHole
  deriving (Eq, Show)

objectMeta :: HeapObject -> Maybe StgValue
objectMeta obj = case obj of
  Closure { closureMeta = (MetadataValue v) } -> Just v
  PartialApplication { papMeta = (MetadataValue v) } -> Just v
  _ -> Nothing

instance StgPretty HeapObject where
  prettify (Closure lf env _ _) = prettify env <> P.space <> prettify lf
  prettify (PartialApplication lf env args arity _ _) =
    prettify env <> P.space <>
    P.parens (prettify args <> P.text "..." <> P.int (fromIntegral arity)) <>
    prettify lf
  prettify BlackHole = P.text "•"

-- | Sequence of values, used for both local environment and arrays of
-- resolved arguments.
newtype ValVec =
  ValVec (Seq.Seq StgValue)
  deriving (Eq, Show, Semigroup, Monoid)

toValVec :: [StgValue] -> ValVec
toValVec = ValVec . Seq.fromList

envSize :: ValVec -> Word64
envSize (ValVec v) = fromIntegral $ Seq.length v

singleton :: StgValue -> ValVec
singleton = ValVec . Seq.singleton

extendEnv :: ValVec -> ValVec -> (ValVec, RefVec)
extendEnv env args = (env', rs)
  where
    envlen = envSize env
    arglen = envSize args
    env' = env <> args
    rs = locals envlen (arglen + envlen)

-- | Split a ValVec in two at the specified index
splitVecAt :: Integral a => a -> ValVec -> (ValVec, ValVec)
splitVecAt n (ValVec vs) =
  let (l, r) = Seq.splitAt (fromIntegral n) vs
   in (ValVec l, ValVec r)

instance StgPretty ValVec where
  prettify (ValVec vs) = P.braces $ P.hcat $ map p (toList vs)
    where
      p (StgNat v _) = prettify v
      p (StgAddr _) = P.char '.'

-- | Entry on the frame stack, encoding processing to be done later
-- when a value is available
data Continuation
  = Branch { branchTable :: !BranchTable
           , branchEnv :: !ValVec }
  | Update { updateAddress :: !Address
           , updateMetadata :: !HeapObjectMetadata }
  | ApplyToArgs { pendingArgs :: !ValVec }
  deriving (Eq, Show)

instance StgPretty Continuation where
  prettify (Branch _ _) = P.text "Br"
  prettify (Update _ _) = P.text "Up"
  prettify (ApplyToArgs _) = P.text "Ap"

-- | Continuation stack also records call stacks for restoring them on
-- return
data StackElement = StackElement
  { stackContinuation :: !Continuation
  , stackCallStack :: !CallStack
  } deriving (Eq, Show)


instance StgPretty StackElement where
  prettify (StackElement k _cs) = prettify k

-- | Currently executing code
data Code
  = Eval !StgSyn
         !ValVec
  | ReturnCon !Tag
              !ValVec
              !(Maybe StgValue)
  | ReturnLit !Native
              !(Maybe StgValue)
  | ReturnFun !Address
  deriving (Eq, Show)

instance StgPretty Code where
  prettify (Eval e le) = P.text "EVAL" <+> prettify le <+> prettify e
  prettify (ReturnCon t binds meta) =
    P.text "RETURNCON" <+>
    P.int (fromIntegral t) <+> prettify binds <+> maybe P.empty prettify meta
  prettify (ReturnLit n meta) =
    P.text "RETURNLIT" <+> prettify n <+> maybe P.empty prettify meta
  prettify (ReturnFun _) = P.text "RETURNFUN" <+> P.text "."

-- | MachineStack
newtype MachineStack = MachineStack [StackElement]
  deriving (Show, Eq, Semigroup, Monoid)

-- | Push a continuation onto the stack
push :: MachineState -> Continuation -> MachineState
push ms@MachineState {machineStack = MachineStack st} k =
  let stackElement = StackElement k (machineCallStack ms)
   in ms {machineStack = MachineStack (stackElement : st)}

-- | Pop a continuation off the stack
pop :: MonadThrow m => MachineState -> m (Maybe Continuation, MachineState)
pop ms@MachineState {machineStack = MachineStack []} = return (Nothing, ms)
pop ms@MachineState {machineStack = MachineStack st} =
  let StackElement k cs = head st
   in return
        ( Just k
        , ms {machineStack = MachineStack (tail st), machineCallStack = cs})


-- | Machine state.
--
data MachineState = MachineState
  { machineCode :: Code
    -- ^ Next instruction to execute
  , machineGlobals :: !(HashMap String StgValue)
    -- ^ Global (heap allocated) objects
  , machineStack :: !MachineStack
    -- ^ stack of continuations
  , machineCounter :: !Int
    -- ^ count of steps executed so far
  , machineTerminated :: !Bool
    -- ^ whether the machine has terminated
  , machineTrace :: MachineState -> IO ()
    -- ^ debug action to run prior to each step
  , machineEvents :: ![Event]
    -- ^ events fired by last step
  , machineEmit :: MachineState -> Event -> IO MachineState
    -- ^ emit function to send out events
  , machineDebug :: !Bool
    -- ^ debug checks on
  , machineDebugEmitLog :: ![Event]
    -- ^ log of emitted events for debug / testing
  , machineLastStepName :: !String
    -- ^ current call stack for debugging
  , machineCallStack :: !CallStack
  }

-- | Call the machine's trace function
traceOut :: MonadIO m => MachineState -> m ()
traceOut ms@MachineState {machineTrace = tr} = liftIO $ tr ms

-- | Clear events and prepare for next step
prepareStep :: MonadIO m => String -> MachineState -> m MachineState
prepareStep stepName ms =
  traceOut ms >>
  return (tick $ ms {machineEvents = mempty, machineLastStepName = stepName})

allocGlobal :: MonadIO m => String -> LambdaForm -> m StgValue
allocGlobal _name impl =
  liftIO $
  StgAddr <$>
  allocate
    Closure
      { closureCode = impl
      , closureEnv = mempty
      , closureCallStack = mempty
      , closureMeta = MetadataPassThrough
      }

-- | Initialise machine state.
initMachineState ::
     MonadIO m => StgSyn -> HashMap String LambdaForm -> m MachineState
initMachineState stg ge = do
  genv <- liftIO $ HM.traverseWithKey allocGlobal ge
  return
    MachineState
      { machineCode = Eval stg mempty
      , machineGlobals = genv
      , machineStack = mempty
      , machineCounter = 0
      , machineTerminated = False
      , machineTrace = \_ -> return ()
      , machineEvents = mempty
      , machineEmit = \s _ -> return s
      , machineDebug = False
      , machineDebugEmitLog = []
      , machineLastStepName = "<INIT>"
      , machineCallStack = mempty
      }

-- | Dump machine state for debugging.
instance StgPretty MachineState where
  prettify MachineState { machineCode = code
                        , machineGlobals = _globals
                        , machineStack = MachineStack stack
                        , machineCounter = counter
                        , machineDebugEmitLog = events
                        , machineLastStepName = step
                        , machineCallStack = cs
                        } =
    P.vcat
      [ (P.int counter <> P.colon) <+>
        (P.text step <> P.text "-->") <+>
        P.parens (P.hcat (P.punctuate P.colon (map prettify (reverse stack)))) <+>
        prettify cs
      , P.nest 4 $ P.space $+$ prettify code $+$ P.space
      , if null events
          then P.empty
          else P.nest 2 (P.text ">>> " <> P.text (show events))
      , P.space
      ]


-- | Throw STG error, passing in current call stack
throwIn :: MonadThrow m => MachineState -> StgError -> m a
throwIn ms err = throwM $ StgException err (machineCallStack ms)


-- | Lookup address of global
globalAddress :: MonadThrow m => MachineState -> String -> m StgValue
globalAddress ms@MachineState {machineGlobals = g} nm =
  case HM.lookup nm g of
    Just v -> return v
    _ -> throwIn ms $ UnknownGlobal nm


-- | Resolve environment references against local and global
-- environments.
val :: MonadThrow m => ValVec -> MachineState -> Ref -> m StgValue
val (ValVec le) ms (Local l) =
  case le Seq.!? fromIntegral l of
    Just v -> return v
    _ -> throwIn ms $ EnvironmentIndexOutOfRange $ fromIntegral l
val _ ms (Global nm) = globalAddress ms nm
val _ _ (Literal n) = return $ StgNat n Nothing

-- | Resolve a vector of refs against an environment to create
-- environment
vals :: MonadThrow m => ValVec -> MachineState -> Seq.Seq Ref -> m ValVec
vals le ms rs = ValVec <$> traverse (val le ms) rs

-- | Resolve a ref against env and machine to get address of
-- HeapObject
resolveHeapObject :: MonadThrow m => ValVec -> MachineState -> Ref -> m Address
resolveHeapObject env ms ref =
  val env ms ref >>= \case
    StgAddr r -> return r
    StgNat _ _ -> throwIn ms NonAddressStgValue

-- | Resolve a ref against env and machine
resolveNative :: MonadThrow m => ValVec -> MachineState -> Ref -> m Native
resolveNative env ms ref =
  val env ms ref >>= \case
    StgAddr _ -> throwIn ms NonNativeStgValue
    StgNat n _ -> return n

-- | Increase counters
tick :: MachineState -> MachineState
tick ms@MachineState {machineCounter = n} = ms {machineCounter = n + 1}


-- | Set the next instruction
setCode :: MachineState -> Code -> MachineState
setCode ms@MachineState {} c = ms {machineCode = c}

-- | Set the name of the last rule executed
setRule :: String -> MachineState -> MachineState
setRule r ms@MachineState {} = ms {machineLastStepName = r}

infixl 5 //?
(//?) :: MachineState -> String -> MachineState
(//?) ms@MachineState {} r =  ms {machineLastStepName = r}

-- | Set the current call stack
setCallStack :: CallStack -> MachineState -> MachineState
setCallStack cs ms = ms { machineCallStack = cs }

-- | Append annotation to current
-- call stack
appendCallStack :: (String, SMID) -> MachineState -> MachineState
appendCallStack ann ms@MachineState {machineCallStack = cs} =
  ms {machineCallStack = addEntry ann cs}

-- | Append event for this step
appendEvent :: Event -> MachineState -> MachineState
appendEvent e ms@MachineState {machineEvents = es0} =
  ms {machineEvents = e : es0}

-- | Build a closure from a STG PreClosure
buildClosure ::
     MonadThrow m => ValVec -> MachineState -> PreClosure -> m HeapObject
buildClosure le ms (PreClosure captures metaref code) = do
  env <- vals le ms captures
  meta <- case metaref of
    Nothing -> return MetadataPassThrough
    Just r -> MetadataValue <$> val le ms r
  return $ Closure code env (machineCallStack ms) meta

-- | Allocate new closure. Validate env refs if we're in debug mode.
allocClosure :: ValVec -> MachineState -> PreClosure -> IO Address
allocClosure le ms cc = buildClosure le ms cc >>= allocate
