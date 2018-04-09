module Eucalypt.Core.Builtin where

import Eucalypt.Core.Syn
import Eucalypt.Core.EvalByName

-- $ errors
--
--

data RuntimeError = RuntimeError String
  deriving (Show)

type Result a = Either RuntimeError a

runtimeError s = Left $ RuntimeError s

-- $ lists
--
--

euCar :: CoreExp a -> Result (CoreExp a)
euCar = undefined -- TODO: implement car


euCdr :: CoreExp a -> Result (CoreExp a)
euCdr = undefined -- TODO: implement cdr


-- | Concatenate two lists or blocks into a list. The default action
-- for list concatenation and also available as `concat`.
euConcat :: Show a => CoreExp a -> CoreExp a -> Result (CoreExp a)
euConcat l r = do
  items <- (++) <$> (extract l) <*> (extract r)
  return (CoreList items)

  where extract e = case whnf e of
          CoreList items -> Right items
          CoreBlock (CoreList items) -> Right items
          _ -> runtimeError ("concat argument " ++ show e ++ "not a list")


-- | Block merge. The default action for block catentation (or
-- applying a block as a function). Also available as `merge`.
euMerge :: Show a => CoreExp a -> CoreExp a -> Result (CoreExp a)
euMerge l r = euConcat l r >>= (return . CoreBlock)
