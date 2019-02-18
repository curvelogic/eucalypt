{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase, PatternSynonyms, ViewPatterns, ScopedTypeVariables #-}
{-|
Module      : Eucalypt.Stg.Loaders
Description : Utils for scraping haskell values out of STG machine memory
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Scrapers where

import qualified Data.Sequence as Seq
import Data.Symbol
import Eucalypt.Stg.Address
import Eucalypt.Stg.Error
import Eucalypt.Stg.Machine
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn
import Eucalypt.Stg.Tags

pattern Empty :: Seq.Seq a
pattern Empty <- (Seq.viewl -> Seq.EmptyL)

pattern (:<) :: a -> Seq.Seq a -> Seq.Seq a
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)

class Scrapeable a where
  -- | Inspect machine state to retrieve haskell value of type 'a'
  -- from machine value (native / address)
  scrape :: MachineState -> StgValue -> IO (Maybe a)

instance Scrapeable StgValue where
  scrape _ms = return . Just

instance Scrapeable Native where
  scrape _ms (StgNat n _) = return $ Just n
  scrape ms (StgAddr a) = do
    obj <- peek a
    case obj of
      Closure {closureEnv = e, closureCode = LambdaForm {lamBody = (Atom r)}} ->
        let v = value (e, ms) (nativeToValue <$> r)
         in scrape ms v
      _ -> return Nothing

instance Scrapeable Symbol where
  scrape ms v = scrape ms v >>= \case
    (Just (NativeSymbol sym)) -> return $ Just sym
    _ -> return Nothing

instance Scrapeable a => Scrapeable [a] where
  scrape ms (StgNat n _) = throwIn ms $ IntrinsicExpectedListFoundNative n
  scrape ms (StgAddr addr) = do
    obj <- peek addr
    case obj of
      Closure { closureEnv = e
              , closureCode = LambdaForm {lamBody = (App (Con TagCons) xs)}
              } -> do
        let (h :< (t :< _)) = asSeq $ values (e, ms) $ nativeToValue <$> xs
        h' <- scrape ms h :: IO (Maybe a)
        t' <- scrape ms t :: IO (Maybe [a])
        return $ (:) <$> h' <*> t'
      Closure {closureCode = LambdaForm {lamBody = (App (Con TagNil) _)}} ->
        return $ Just []
      Closure {closureCode = lf} ->
        throwIn ms $ IntrinsicExpectedEvaluatedList (lamBody lf)
      BlackHole -> throwIn ms IntrinsicExpectedListFoundBlackHole
      PartialApplication {} ->
        throwIn ms IntrinsicExpectedListFoundPartialApplication

instance (Scrapeable k, Scrapeable v) => Scrapeable (k, v) where
  scrape ms (StgNat n _) = throwIn ms $ IntrinsicExpectedListFoundNative n
  scrape ms (StgAddr addr) = do
    obj <- peek addr
    case obj of
      Closure { closureEnv = e
              , closureCode = LambdaForm {lamBody = (App (Con TagCons) xs)}
              } -> do
        let (h :< (t :< _)) = asSeq $ values (e, ms) $ nativeToValue <$> xs
        k <- scrape ms h :: IO (Maybe k)
        t' <- scrape ms t :: IO (Maybe [v])
        case t' of
          (Just (v:_)) -> return $ (,) <$> k <*> Just v
          _ -> throwIn ms IntrinsicBadPair
      Closure {closureCode = lf} ->
        throwIn ms $ IntrinsicExpectedEvaluatedList (lamBody lf)
      BlackHole -> throwIn ms IntrinsicExpectedListFoundBlackHole
      PartialApplication {} ->
        throwIn ms IntrinsicExpectedListFoundPartialApplication


data BareCons k = BareCons
  { car :: k
  , cdr :: StgValue
  }

asTuple :: BareCons k -> (k, StgValue)
asTuple BareCons{..} = (car, cdr)

instance Scrapeable k => Scrapeable (BareCons k) where
  scrape ms (StgNat n _) = throwIn ms $ IntrinsicExpectedListFoundNative n
  scrape ms (StgAddr addr) = do
    obj <- peek addr
    case obj of
      Closure { closureEnv = e
              , closureCode = LambdaForm {lamBody = (App (Con TagCons) xs)}
              } -> do
        let (h :< (t :< _)) = asSeq $ values (e, ms) $ nativeToValue <$> xs
        k <- scrape ms h
        return $ BareCons <$> k <*> pure t
      Closure {closureCode = lf} ->
        throwIn ms $ IntrinsicExpectedEvaluatedList (lamBody lf)
      BlackHole -> throwIn ms IntrinsicExpectedListFoundBlackHole
      PartialApplication {} ->
        throwIn ms IntrinsicExpectedListFoundPartialApplication
