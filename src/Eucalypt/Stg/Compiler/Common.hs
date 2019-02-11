{-# LANGUAGE Rank2Types #-}
{-|
Module      : Eucalypt.Stg.Compiler.Common
Description : Common code for compilation
Copyright   : (c) Greg Hawkins, 2018
License     :
Maintainer  : greg@curvelogic.co.uk
Stability   : experimental

-}

module Eucalypt.Stg.Compiler.Common where

import Data.Scientific
import Data.Symbol
import Eucalypt.Core.Syn as C
import Eucalypt.Core.SourceMap (SMID)
import Eucalypt.Stg.GlobalInfo
import Eucalypt.Stg.Native
import Eucalypt.Stg.Syn

-- | Convert a core primitive into a literal ref
literal :: C.Primitive -> Ref
literal (CoreInt n) = V $ NativeNumber $ fromIntegral n
literal (CoreFloat d) = V $ NativeNumber $ fromFloatDigits d
literal (CoreSymbol s) = V $ NativeSymbol $ intern s
literal (CoreString s) = V $ NativeString s
literal (CoreBoolean True) = gref "TRUE"
literal (CoreBoolean False) = gref "FALSE"
literal CoreNull = gref "NULL"

-- | Annotate the generated syntax with a name an source map id if a
-- name is provided.
annotate :: Maybe CoreBindingName -> SMID -> StgSyn -> StgSyn
annotate m smid syn = case m of
  (Just m') -> ann_ m' smid syn
  Nothing -> syn
