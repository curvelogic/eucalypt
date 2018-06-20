{-|
Module      : Eucalypt.Stg.Machine
Description : Spineless tagless G-machine
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

Heavily based on
https://github.com/ermine-language/ermine/blob/master/src/Ermine/Interpreter.hs
(-- Copyright :  (c) Edward Kmett and Dan Doel 2014)
-}
module Eucalypt.Stg.Machine where

import Control.Applicative
import Control.Monad.Primitive
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.Primitive.MutVar
import Data.Vector (Vector)
import qualified Data.Vector as B
import Data.Vector.Generic ((!))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as BM
import Data.Word
import Eucalypt.Stg.Syn
import Prelude hiding (log)

newtype Address m =
  Address (MutVar (PrimState m) (Closure m))
  deriving (Eq)

data Closure m
  = Closure { _closureCode :: !LambdaForm
            , _closureEnv :: !(Env m) }
  | PartialApplication { _closureCode :: !LambdaForm
                       , _closureEnv :: !(Env m)
                       , _papArity :: !Int -- remaining args
                        }
  | BlackHole
  | PrimClosure (MachineState m -> m ())

newtype Env m =
  Env (Vector (Address m))

data Frame m
  = Branch !Continuation
           !(Env m)
  | Update !(Address m)

data MachineState m = MachineState
  { _sp :: !Int
  , _fp :: !Int
  , _stackF :: [(Int, Frame m)]
  , _genv :: HashMap Word64 (Address m)
  , _trace :: String -> m ()
  , _stack :: BM.MVector (PrimState m) (Address m)
  }

defaultMachineState ::
     (Applicative m, PrimMonad m)
  => Int
  -> HashMap Word64 (Address m)
  -> m (MachineState m)
defaultMachineState stackSize ge =
  MachineState stackSize stackSize [] ge (const $ return ()) <$>
  GM.replicate stackSize sentinel

log :: MachineState m -> String -> m ()
log MachineState {_trace = trace} = trace

note :: (Monad m, Show a) => MachineState m -> String -> a -> m ()
note ms n a = log ms (n ++ ": " ++ show a)

sentinel :: a
sentinel = error "PANIC: access past end of stack"

buildClosure ::
     (PrimMonad m, Applicative m)
  => Env m
  -> PreClosure
  -> MachineState m
  -> m (Closure m)
buildClosure le (PreClosure captures code) ms =
  Closure code <$> resolveEnv le captures ms

allocClosure ::
     (Applicative m, PrimMonad m)
  => Env m
  -> PreClosure
  -> MachineState m
  -> m (Address m)
allocClosure le cc ms = Address <$> (buildClosure le cc ms >>= newMutVar)

-- | Resolve a ref against env and machine to get address
resolveClosure ::
     (PrimMonad m) => Env m -> MachineState m -> Ref -> m (Address m)
resolveClosure (Env le) _ (Local l) = return $ le ! fromIntegral l
resolveClosure _ MachineState {_stack = st, _sp = sp} (Stack l) =
  GM.read st (fromIntegral l + sp)
resolveClosure _ MachineState {_genv = genv} (Global g) =
  return $ genv HM.! fromIntegral g

-- | Resolve a vector of refs against an environment to create
-- environment
resolveEnv ::
     (Applicative m, PrimMonad m)
  => Env m
  -> Vector Ref
  -> MachineState m
  -> m (Env m)
resolveEnv le refs ms = Env <$> G.mapM (resolveClosure le ms) refs

-- | Move stack pointer to make space and fill with the argument
-- addresses.
pushArgs ::
     (Applicative m, PrimMonad m)
  => Env m
  -> Vector Ref
  -> MachineState m
  -> m (MachineState m)
pushArgs le refs ms@MachineState {_sp = sp, _stack = stack} = do
  let len = B.length refs
  note ms "pushArgs" len
  let sp' = sp - len
  let ms' = ms {_sp = sp'}
  forM_ (B.zip (B.fromList [sp' ..]) refs) $ \(i, r) ->
    resolveClosure le ms r >>= GM.write stack (sp' + i)
  return ms'

copyArgs :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> Int -> Int -> Int -> m ()
copyArgs stk frm off len =
  GM.move (GM.slice (frm + off - len) len stk) (GM.slice frm len stk)

-- | Squash up the stack.
squash ::
     PrimMonad m
  => Int
  -> Int
  -> MachineState m
  -> m (MachineState m)
squash sz args ms@MachineState {_stack = stack, _sp = sp} = do
  note ms "squash" (sz, args)
  copyArgs stack sp sz args
  when (sz > args) $ GM.set (GM.slice sp (sz - args) stack) sentinel
  return $ ms {_sp = sp + (sz - args)}

eval ::
     (Applicative m, PrimMonad m) => StgSyn -> Env m -> MachineState m -> m ()
eval (App sz f xs) le ms =
  case f of
    Ref r -> do
      addr <- resolveClosure le ms r
      ms' <- pushArgs le xs ms
      let lxs = G.length xs
      let lsz = fromIntegral sz
      ms'' <- squash (lsz + lxs) lxs ms'
      enter addr ms''
    Con t -> pushArgs le xs ms >>= returnCon t (G.length xs)

-- eval (Let bs e) le MachineState{_stack = stack} = do
--   let len = G.length bs
eval _ _ _= undefined


enter :: (Applicative m, PrimMonad m) => Address m -> MachineState m -> m ()
enter = undefined

returnCon ::
     (Applicative m, PrimMonad m) => Tag -> Int -> MachineState m -> m ()
returnCon = undefined
