{-|
Module      : Eucalypt.Stg.Syn
Description : Syntax for spineless tagless G-machine execution
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

Heavily based on
https://github.com/ermine-language/ermine/blob/master/src/Ermine/Syntax/G.hs
(-- Copyright :  (c) Edward Kmett and Dan Doel 2014)
-}
module Eucalypt.Stg.Syn where

import Data.Map
import Data.Vector
import Data.Word

data Ref
  = Global !Word64
  | Local !Word64
  | Stack !Word64
  deriving (Show)

type Tag = Word64

data Continuation =
  Continuation (Map Tag (Word64, StgSyn))
               (Maybe StgSyn)
  deriving (Show)

data Func
  = Ref !Ref -- closure ref
  | Con !Tag
  deriving (Show)

data LambdaForm = LambdaForm
  { _free :: !Word64
  , _bound :: !Word64
  , _update :: !Bool
  , _body :: !StgSyn
  } deriving (Show)

data PreClosure =
  PreClosure !(Vector Ref)
             !LambdaForm
  deriving (Show)

data StgSyn
  = Case !StgSyn
         !Continuation
  | CaseLit !Ref
            !Continuation
  | App !Word64
        !Func
        !(Vector Ref)
  | Let (Vector PreClosure)
        !StgSyn
  | LetRec (Vector PreClosure)
           !StgSyn
  | Slot
  deriving (Show)

-- | Create a non-updatable lambda form
noUpdate :: Word64 -> Word64 -> StgSyn -> LambdaForm
noUpdate f b = LambdaForm f b False

-- | Create an updatable (i.e. thunk) lambda form
doUpdate :: Word64 -> StgSyn -> LambdaForm
doUpdate f = LambdaForm f 0 True

-- | Standard constructor - applies saturated data constructor of tag
-- @t@ to refs on stack.
standardConstructor :: Word64 -> Tag -> LambdaForm
standardConstructor f t =
  LambdaForm f 0 False $
  App 0 (Con t) $ generate (fromIntegral f) $ Local . fromIntegral
